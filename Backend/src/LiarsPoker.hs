{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------
-- |
-- Game logic for LiarsPoker multiplayer game.
-- According to the rules: http://www.liars-poker.com
-- (c) 2016 Jeffrey Rosenbluth
----------------------------------------------------------

module LiarsPoker
  ( cardsPerHand
  , numOfPlayers
  , displayHand
  , newGame
  , addPlayer
  , bonus
  , legal
  , mkBid
  , nextPlayer
  , resetGame
  , countRank
  , scoreGame
  ) where

import           Types

import           Control.Lens ((^.), (&), (%~), (.~), over)
import           Data.List    (intersperse)
import           Data.Maybe
import           Data.Text    (Text)
import qualified Data.Vector  as V
import           Data.Vector  (Vector, (!?), snoc, imap)
import           Prelude      hiding (lookup)

cardsPerHand :: Int
cardsPerHand = 8

numOfPlayers :: Game a -> Int
numOfPlayers g = V.length $ g ^. players

-- | Total number of Rank in the game.
countRank :: Hands -> Rank -> Int
countRank hs rank = sum $ getCount rank <$> hs

getCount :: Rank -> Hand -> Int
getCount rank hand = length $ filter (== rank) hand

-- | The order of the cards does not really have to be random, just look random.
--   So we use a trick of setting the seed of the generator to the hand itself.
displayHand :: Hand -> String
displayHand = intersperse ' ' . concatMap show

-- | Create a new game from a gameId and a number of invited players.
newGame :: Int -> Int -> Game (Vector Hand)
newGame i n = Game
  { _players    = mempty
  , _bidder     = Nothing
  , _bid        = (Bid 0 0)
  , _turn       = 0
  , _won        = Nothing
  , _rebid      = False
  , _inProgress = False
  , _baseStake  = 1
  , _gameId     = i
  , _numPlyrs   = n
  , _variant    = mempty
  , _multiple   = 1
  }

resetGame :: Int -> Game a -> Game a
resetGame n g = g & bidder .~ Nothing
                & bid .~ Bid 0 0
                & turn .~ fromMaybe (n `mod` numOfPlayers g) (g ^. bidder)
                & won .~ Nothing
                & rebid .~ False
                & inProgress .~ True

addPlayer :: Game a -> Text -> Game a
addPlayer game nm = game & players %~ flip snoc player
  where
    pId    = numOfPlayers game
    player = Player pId nm 0 (Flags False False False False) Nothing

-- | Change bid to (Bid Rank Int), update the turn to the next player, and
--   set the rebid flag.
mkBid :: Game a -> Bid -> Game a
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
nextPlayer :: Game a -> Game a
nextPlayer game = game & turn %~ (\x -> (x + 1) `mod` numPlayers)
  where
    numPlayers = numOfPlayers game

-- | Is this 'Action' legal to take from the current game state?
legal :: Game a -> Action -> Int -> Bool
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
  Count     -> maybe False (== t) bd && pId == game ^. turn && game ^. won == Nothing
  Say _     -> True
  Invalid _ -> False
  where
    bd = game ^. bidder
    t  = game ^. turn

-- | If the game is over (.i.e. game ^. won = Just _) then return
--   the bonus multiplier. Both the n+3 rule and the Sixes rule.
bonus :: Game a -> Int
bonus game = sixes * mult
  where
    Bid r q = game ^. bid
    mult
      | q < numPlayers + 3 = 1
      | otherwise = (2 + (q - numPlayers - 3) `div` 2)
    sixes
      | (r == 6) = 2
      | otherwise = 1
    numPlayers = numOfPlayers game

-- | The hero bump is 1 if the bidder wins with none.
hero :: Game (Vector Hand) -> Int
hero game
  | (q == 0 && countRank (game ^. variant) bc > 0) = 1
  | otherwise = 0
  where
    Just bdr = game ^. bidder
    q = fromMaybe 0 $ length . (filter (== bc)) <$> (game ^. variant) !? bdr
    bc = game ^. bid ^. bidRank


-- | Score the game and set the new 'baseStake' in accordance with Progressive
--   Stakes.
scoreGame :: Game (Vector Hand) -> Game (Vector Hand)
scoreGame game =
  game & players %~ imap (\i p -> over score (+ (if (bdr == Just i) then b else a)) p)
       & baseStake .~ (if h == 1 then 2 else bns)
       & inProgress .~ False
  where
    bdr    = game ^. bidder
    (a, b) =
      maybe (0, 0)
            (\w -> if w then (-winStake, winB) else (lossStake, -lossB))
            (game ^. won)

    -- The n+3 and sixes rules
    bns = bonus game

    -- The skunk rule
    mult
      | countRank (game ^. variant) (game ^. bid . bidRank) == 0 =
          max 1 (2 * numOfPlayers game - 6)
      | otherwise = bns

    -- The hero bump
    h = hero game

    -- The score for non bidders
    winStake  = (mult + h) * game ^. baseStake
    lossStake = game ^. baseStake

    -- The score for the bidder
    winB  = winStake  * (numOfPlayers game - 1)
    lossB = lossStake * (numOfPlayers game - 1)
