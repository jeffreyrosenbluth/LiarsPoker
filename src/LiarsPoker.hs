{-# LANGUAGE TemplateHaskell #-}

module LiarsPoker
  ( newGame
  , count
  , move
  , legal
  ) where

import           Control.Lens
import           Control.Monad (replicateM)
import           Control.Monad.Random
import           Data.List (zipWith4)
import           Data.List.Split  (chunksOf)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           System.Random

data Card = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C0
  deriving (Show, Eq, Ord, Enum, Bounded)

type Hand = Map Card Int

data Bid = Bid
  { _bidCard  :: Card
  , _bidQuant :: Int
  } deriving (Show, Eq)

instance Ord Bid where
  Bid c1 q1 <= Bid c2 q2 = (q1, c1) <= (q2, c2)

makeLenses ''Bid

data Player = Player
  { _idNum :: Int
  , _name  :: String
  , _hand  :: Hand
  , _score :: Int
  } deriving (Show, Eq)
makeLenses ''Player

data Game = Game
  { _numOfPlayers :: Int
  , _players      :: [Player]
  , _bidder       :: Maybe Player
  , _bid          :: Bid
  , _turn         :: Player
  , _won          :: Maybe Bool
  , _rebid        :: Bool
  } deriving Show
makeLenses ''Game

data Action
  = Raise Bid
  | Challenge
  | Count
  deriving Show
makePrisms ''Action

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
count game card = sum $ map (getCount card) (map (view hand) $ game ^. players)
  where
    getCount c h = fromMaybe 0 (M.lookup c h)

newGame :: Int      -- ^ seed to random number generator.
        -> [String] -- ^ player names
        -> Game
newGame seed []    = error "A game must have players."
newGame seed names =
  Game (length names)
       thePlayers
       Nothing
       (Bid minBound 0)
       theTurn
       Nothing
       False
    where
      numPlayers   = length names
      theHands     = map toMap cards
      thePlayers   = zipWith4 Player [0..] names theHands (repeat 0)
      theTurn : _  = thePlayers
      toMap xs     = foldr (\n -> M.insertWith (+) (int2Card n) 1) M.empty xs
      cards        = chunksOf cardsPerHand
                   $ evalRand (replicateM (numPlayers * cardsPerHand)
                   $ getRandomR (0, 9)) (mkStdGen seed)

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
nextPlayer game = game & turn .~ p
  where
    p = (game ^. players) !! ((n + 1) `mod` numPlayers)
    n = game ^. turn . idNum
    numPlayers = game ^. numOfPlayers

move :: Game -> Action -> Game
move game action = case action of
  Raise b   -> mkBid game b
  Challenge -> nextPlayer game
  Count     -> game & won .~ Just result
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

game2 = newGame 2423    ["sonny", "cher"]
game5 = newGame 7824391 ["alice", "bob", "charlie", "Daniel", "Edward"]
