{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LiarsPoker
  ( Card(..)
  , Bid(..), bidCard, bidQuant
  , Player(..), name, hand, score
  , Game(..), gameId, numOfPlayers, players, bidder, bid, turn, won, rebid
  , Action(..), _Raise, _Challenge, _Count

  , newGame
  , getHand
  , getBid
  , mkBid
  , count
  , move
  , legal
  , value
  , scores

   -- delete before release
  , game2, game5

  ) where

import           Control.Lens
import           Control.Monad (replicateM)
import           Control.Monad.Random
import           Data.List.Split  (chunksOf)
import           Data.Aeson
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           GHC.Generics
import           System.Random

data Card = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C0
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON Card
instance FromJSON Card

type Hand = Map Card Int

instance ToJSON Hand where
  toJSON = toJSON . M.toList

data Bid = Bid
  { _bidCard  :: Card
  , _bidQuant :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Bid
instance FromJSON Bid

instance Ord Bid where
  Bid c1 q1 <= Bid c2 q2 = (q1, c1) <= (q2, c2)

makeLenses ''Bid

data Player = Player
  { _name  :: String
  , _hand  :: Hand
  , _score :: Int
  } deriving (Show, Eq, Generic)
makeLenses ''Player

instance ToJSON Player

data Game = Game
  { _gameId       :: Integer
  , _numOfPlayers :: Int
  , _players      :: [Player]
  , _bidder       :: Maybe Int  -- ^ playerId
  , _bid          :: Bid
  , _turn         :: Int        -- ^ playerId
  , _won          :: Maybe Bool
  , _rebid        :: Bool
  } deriving (Show, Generic)
makeLenses ''Game

instance ToJSON Game

data Action
  = Raise Bid
  | Challenge
  | Count
  deriving (Show, Generic)
makePrisms ''Action

instance ToJSON Action
instance FromJSON Action

cardsPerHand :: Int
cardsPerHand = 8

int2Card :: Int -> Card
int2Card n = case n of
  1 -> C1
  2 -> C2
  3 -> C3
  4 -> C4
  5 -> C5
  6 -> C6
  7 -> C7
  8 -> C8
  9 -> C9
  0 -> C0
  _ -> error "Tried to convert a number outisde of 0-9 to Card."

count :: Game -> Card -> Int
count game card = sum $ getCount . view hand <$> game ^. players
  where
    getCount h = fromMaybe 0 (M.lookup card h)

-- | Given a game and a playerId, return the players hand if the playerId exists.
getHand :: Game -> Int -> Maybe Hand
getHand game pId = view hand <$> game ^. players ^? ix pId

-- | Get the playerId of the bidder and his bid.
getBid :: Game -> Maybe (Int, Bid)
getBid game = ( , game ^. bid) <$> game ^. bidder

newGame :: StdGen   -- ^ Random number generator.
        -> Integer  -- ^ Game Id.
        -> [String] -- ^ player names
        -> Game
newGame _ _ []    = error "A game must have players."
newGame sg gId names =
  Game gId
       (length names)
       thePlayers
       Nothing
       (Bid minBound 0)
       0
       Nothing
       False
    where
      numPlayers   = length names
      theHands     = map toMap cards
      thePlayers   = zipWith3 Player names theHands (repeat 0)
      toMap xs     = foldr (\n -> M.insertWith (+) (int2Card n) 1) M.empty xs
      cards        = chunksOf cardsPerHand
                   $ evalRand (replicateM (numPlayers * cardsPerHand)
                   $ getRandomR (0, 9)) sg

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
    numPlayers = game ^. numOfPlayers

move :: Game -> Action -> Game
move game action = case action of
  Raise b   -> mkBid game b
  Challenge -> nextPlayer game
  Count     -> scores $ game & won .~ Just result
    where
      result = game ^. bid . bidQuant <= count game (game ^. bid . bidCard)

legal :: Game -> Action -> Bool
legal game action = case action of
  -- You can't raise a rebid os if you are the bidder and rebid is True
  -- it is illegal to bid again.
  Raise b   -> not (game ^. rebid && Just t == bd) && b > game ^. bid
  Challenge -> maybe False (/= t) bd
  Count     -> maybe False (== t) bd
  where
    bd = game ^. bidder
    t  = game ^. turn

-- | If the game is over (.i.e. game ^. won = Just _) then return
--   (score for non-bidders, score for bidder).
value :: Game -> (Int, Int)
value game = (factor, factor * (numPlayers - 1))
  where
    factor     = sixes * mult
    Bid c n    = game ^. bid
    mult       = if n < numPlayers + 3 then 1 else 2 + (n - numPlayers - 3) `div` 2
    sixes      = if c == C6 then 2 else 1
    numPlayers = game ^. numOfPlayers

scores :: Game -> Game
scores game = game & players .~ (reScore <$> [0..(game ^. numOfPlayers - 1)])
  where
    reScore p
      | game ^. bidder == Just p =
          (over score (+ x)) (game ^. players . singular (ix p))
      | otherwise =
          (over score (+ a)) (game ^. players . singular (ix p))
    (a , x)   = maybe (0, 0) (\w -> if w then (-ps, b) else (ps, -b)) (game ^. won)
    (ps, b)   = value game

game2 = newGame (mkStdGen 0) 0 ["sonny", "cher"]
game5 = newGame (mkStdGen 1) 1 ["alice", "bob", "charlie", "Daniel", "Edward"]
