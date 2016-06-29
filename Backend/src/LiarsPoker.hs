{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module LiarsPoker where

import           Types

import           Control.Lens hiding ((.=))
import           Data.List    (intersperse)
import qualified Data.Map     as M
import           Data.Maybe
import           Data.Text    (Text)
import           Data.Vector  ((!?))
import qualified Data.Vector  as V

cardsPerHand :: Int
cardsPerHand = 8

numOfPlayers :: Game -> Int
numOfPlayers g = V.length $ g ^. players

-- | Total number of Card in the game.
countCard :: Hands -> Card -> Int
countCard hands card = sum $ getCount <$> hands
  where
    getCount h = fromMaybe 0 (M.lookup card h)

-- | Given a game and a playerId, return the players name if the playerId exists.
getPlayerName :: Game -> Int -> Text
getPlayerName game pId =
   fromMaybe "Error: getPlayerName" $ game ^? players . ix pId . name

-- | Return the players hand if the playerId exists.
getHand :: Hands -> Int -> Hand
getHand hands pId = fromMaybe M.empty (hands !? pId)

toHand :: [Int] -> Hand
toHand = foldr (\n -> M.insertWith (+) n 1) M.empty

displayHand :: Hand -> String
displayHand h = intersperse ' ' $ M.foldrWithKey f "" h
  where
    f k a b = replicate a (head $ show k) ++ b

getBidderName :: Game -> Text
getBidderName g =
  maybe "Error: getBidderName"
        (\i -> g ^. players . ix i . name)
        (g ^. bidder)

getTurnName :: Game -> Text
getTurnName g = ps ^. ix b . name
  where
    b = g ^. turn
    ps = g ^. players

-- | Create a new game from a gameId and a number of invited players.
newGame :: Int -> Int -> Game
newGame = Game V.empty Nothing (Bid 0 0) 0 Nothing False False 1

resetGame :: Int -> Game -> Game
resetGame n g = g & bidder .~ Nothing
                & bid .~ Bid 0 0
                & turn .~ fromMaybe (n `mod` numOfPlayers g) (g ^. bidder)
                & won .~ Nothing
                & rebid .~ False
                & inProgress .~ True

addPlayer :: Game -> Int -> Text -> Game
addPlayer game pId nm = game & players %~ flip V.snoc player
  where
    player = Player pId nm 0

dealHands :: [[Int]] -> Hands
dealHands cs = V.fromList (toHand <$> cs)

-- | Change bid to (Bid Card Int) and update the turn to the next player.
mkBid :: Game -> Bid -> Game
mkBid game b = nextPlayer
             $ game & bidder .~ p
                    & bid    .~ b
                    & rebid  .~ r
  where
    p = Just $ game ^. turn
    -- If the player whose bidding is the previous bidder than this is
    -- a rebid.
    r = game ^. bidder == Just (game ^. turn)

-- | Move the turn to the next player.
nextPlayer :: Game -> Game
nextPlayer game = game & turn %~ (\x -> (x + 1) `mod` numPlayers)
  where
    numPlayers = numOfPlayers game

-- | Is this 'Action' legal to take from the current game state?
legal :: Game -> Action -> Bool
legal game action = case action of
  Join _ _  -> not (game ^. inProgress)
  New _ _   -> not (game ^. inProgress)
  Deal      -> not (game ^. inProgress)
  -- You can't raise a rebid, if you are the bidder and rebid is True
  -- it is illegal to bid again.
  Raise b   -> not (game ^. rebid && Just t == bd) && b > game ^. bid
  Challenge -> maybe False (/= t) bd
  Count     -> maybe False (== t) bd
  Say _     -> True
  Invalid _ -> False
  where
    bd = game ^. bidder
    t  = game ^. turn

-- | If the game is over (.i.e. game ^. won = Just _) then return
--   the bonus multiplier. Both the n+3 rule and the Sixes rule.
bonus :: Game -> Int
bonus game = sixes * mult
  where
    Bid c n    = game ^. bid
    mult       = if n < numPlayers + 3 then 1 else 2 + (n - numPlayers - 3) `div` 2
    sixes      = if c == 6 then 2 else 1
    numPlayers = numOfPlayers game

-- | The hero bump is 1 if the bidder wins with none.
hero :: Game -> Hands -> Int
hero game hands = if q == 0 && countCard hands bc > 0 then 1 else 0
  where
    Just bdr = game ^. bidder
    q = fromMaybe 0 $ M.lookup bc =<< hands !? bdr
    bc = game ^. bid ^. bidCard


-- | Score the game and set the new 'baseStake' in accordance with Progressive
--   Stakes.
scoreGame :: Game -> Hands -> Game
scoreGame game hands = game & players %~ V.imap reScore
                            & baseStake .~ (if h == 1 then 2 else bns)
  where
    reScore :: Int -> Player -> Player
    reScore idx p
      | game ^. bidder == Just idx = over score (+ x) p
      | otherwise                  = over score (+ a) p

    (a, x)    = maybe (0, 0)
                      (\w -> if w then (-winStake, winB) else (lossStake, -lossB))
                      (game ^. won)
    cnt       = countCard hands (game ^. bid . bidCard)
    -- The n+3 rule.
    bns       = bonus game
    -- The skunk rule.
    mult      = if cnt == 0 then max 1 (2 * numOfPlayers game - 6) else bns
    -- The hero bump.
    h         = hero game hands
    -- The score for non bidders.
    winStake  = (mult + h) * game ^. baseStake
    lossStake = game ^. baseStake
    -- The score for the bidder
    winB      = winStake * (numOfPlayers game - 1)
    lossB     = lossStake * (numOfPlayers game - 1)
