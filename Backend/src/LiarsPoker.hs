{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------
-- |
-- Game logic for LiarsPoker multiplayer game.
-- According to the rules: http://www.liars-poker.com
-- (c) 2016 Jeffrey Rosenbluth
--------------------------------------------------------

module LiarsPoker where

import           Types

import           Control.Lens ((^?), (^.), (&), (%~), (.~), ix, over)
import           Data.List    (intersperse)
import           Data.Map     (insertWith, foldrWithKey, lookup)
import           Data.Maybe
import           Data.Text    (Text)
import           Data.Vector  (Vector, (!?), snoc, imap, length)
import           Prelude      hiding (length, lookup)

-- | if-then-else sugar.
iF :: Bool -> a -> a -> a
iF True t _  = t
iF False _ f = f

cardsPerHand :: Int
cardsPerHand = 8

numOfPlayers :: Game f -> Int
numOfPlayers g = length $ g ^. players

-- | Total number of Rank in the game.
countRank :: Hands -> Rank -> Int
countRank hands rank = sum $ getCount rank <$> hands

getCount :: Rank -> Hand -> Int
getCount rank h = fromMaybe 0 (lookup rank h)

-- | Given a game and a playerId, return the players name if the playerId exists.
getPlayerName :: Game f -> Int -> Maybe Text
getPlayerName game pId = game ^? players . ix pId . name

toHand :: [Int] -> Hand
toHand = foldr (\n -> insertWith (+) n 1) mempty

firstDigit :: Int -> Char
firstDigit n =
  case show n of
    (x:_) -> x
    ""    -> error "The impossible happend, show int == []"

displayHand :: Hand -> String
displayHand h = intersperse ' ' $ foldrWithKey f "" h
  where
    f k a b = replicate a (firstDigit k) ++ b

getBidderName :: Game f -> Text
getBidderName g =
  maybe "Nobody"
        (\i -> g ^. players . ix i . name)
        (g ^. bidder)

-- | Create a new game from a gameId and a number of invited players.
newGame :: Int -> Int -> Game Vector
newGame = Game mempty Nothing (Bid 0 0) 0 Nothing False False 1 mempty

resetGame :: Int -> Game f -> Game f
resetGame n g = g & bidder .~ Nothing
                & bid .~ Bid 0 0
                & turn .~ fromMaybe (n `mod` numOfPlayers g) (g ^. bidder)
                & won .~ Nothing
                & rebid .~ False
                & inProgress .~ True
                & mempty

addPlayer :: Game f -> Text -> Game f
addPlayer game nm = game & players %~ flip snoc player
  where
    pId    = numOfPlayers game
    player = Player pId nm 0

-- | Change bid to (Bid Rank Int), update the turn to the next player, and
--   set the rebid flag.
mkBid :: Game f -> Bid -> Game f
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
nextPlayer :: Game f -> Game f
nextPlayer game = game & turn %~ (\x -> (x + 1) `mod` numPlayers)
  where
    numPlayers = numOfPlayers game

-- | Is this 'Action' legal to take from the current game state?
legal :: Game f -> Action -> Int -> Bool
legal game action pId = case action of
  Join _ _  -> not (game ^. inProgress)
  New  _ _  -> not (game ^. inProgress)
  Deal      -> not (game ^. inProgress)
  -- You can't raise a rebid, if you are the bidder and rebid is True
  -- it is illegal to bid again.
  Raise b   -> not (game ^. rebid && Just t == bd)
                && b > game ^. bid
                && pId ==  game ^. turn
  Challenge -> maybe False (/= t) bd && pId == game ^. turn
  Count     -> maybe False (== t) bd && pId == game ^. turn
  Say _     -> True
  Invalid _ -> False
  where
    bd = game ^. bidder
    t  = game ^. turn

-- | If the game is over (.i.e. game ^. won = Just _) then return
--   the bonus multiplier. Both the n+3 rule and the Sixes rule.
bonus :: Game f -> Int
bonus game = sixes * mult
  where
    Bid r q    = game ^. bid
    mult       = iF (q < numPlayers + 3) 1 (2 + (q - numPlayers - 3) `div` 2)
    sixes      = iF (r == 6) 2 1
    numPlayers = numOfPlayers game

-- | The hero bump is 1 if the bidder wins with none.
hero :: Game f -> Int
hero game hands = iF (q == 0 && countRank hands bc > 0) 1 0
  where
    Just bdr = game ^. bidder
    q = fromMaybe 0 $ lookup bc =<< (game ^. hands) !? bdr
    bc = game ^. bid ^. bidRank


-- | Score the game and set the new 'baseStake' in accordance with Progressive
--   Stakes.
scoreGame :: Game Vector -> Game Vector
scoreGame game =
  game & players   %~ imap (\i p -> over score (+ (iF (bdr == Just i) b a)) p)
       & baseStake .~ iF (h == 1) 2 bns
  where
    bdr    = game ^. bidder
    (a, b) =
      maybe (0, 0)
            (\w -> iF w (-winStake, winB) (lossStake, -lossB))
            (game ^. won)

    -- The n+3 and sixes rules
    bns = bonus game

    -- The skunk rule
    mult =
      iF (countRank (game ^. hands) (game ^. bid . bidRank) == 0)
         (max 1 (2 * numOfPlayers game - 6))
         bns

    -- The hero bump
    h = hero game

    -- The score for non bidders
    winStake  = (mult + h) * game ^. baseStake
    lossStake = game ^. baseStake

    -- The score for the bidder
    winB  = winStake  * (numOfPlayers game - 1)
    lossB = lossStake * (numOfPlayers game - 1)
