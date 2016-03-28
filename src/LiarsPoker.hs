{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LiarsPoker
  ( Card, cardsPerHand
  , Bid(..), bidCard, bidQuant
  , Player(..), name, hand, score
  , Game(..), numOfPlayers, players, bidder, bid, turn, won, rebid
  , Action(..), _Raise, _Challenge, _Count

  , newGame
  , addPlayer
  , dealHands
  , toHand
  , displayHand
  , getHand
  , getBid
  , mkBid
  , nextPlayer
  , count
  , legal
  , value
  , scores
  , inProgress
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics

type Card = Int

type Hand = Map Card Int

instance ToJSON Hand where
  toJSON = toJSON . M.toList

instance FromJSON Hand where
  parseJSON = fmap M.fromList . parseJSON

data Bid = Bid
  { _bidCard  :: Card
  , _bidQuant :: Int
  } deriving (Eq, Generic)

instance Show Bid where
  show (Bid c q) = show q ++ " " ++ show c ++ "'s"

instance ToJSON Bid
instance FromJSON Bid

instance Ord Bid where
  Bid c1 q1 <= Bid c2 q2 = (q1, c1) <= (q2, c2)

makeLenses ''Bid

data Player = Player
  { _playerId :: Int
  , _name  :: Text
  , _hand  :: Hand
  , _score :: Int
  } deriving (Show, Eq, Generic)
makeLenses ''Player

instance ToJSON Player
instance FromJSON Player

data Game = Game
  { _numOfPlayers :: Int
  , _players      :: [Player]
  , _bidder       :: Maybe Int  -- ^ playerId
  , _bid          :: Bid
  , _turn         :: Int        -- ^ playerId
  , _won          :: Maybe Bool
  , _rebid        :: Bool
  , _inProgress   :: Bool
  } deriving (Show, Generic)
makeLenses ''Game

instance ToJSON Game
instance FromJSON Game

data Action
  = SetName Text
  | Deal
  | Raise Bid
  | Challenge
  | Count
  | Say Text
  | Invalid Text
  deriving (Show, Generic)
makePrisms ''Action

instance ToJSON Action
instance FromJSON Action

cardsPerHand :: Int
cardsPerHand = 8

count :: Game -> Card -> Int
count game card = sum $ getCount . view hand <$> game ^. players
  where
    getCount h = fromMaybe 0 (M.lookup card h)

-- | Given a game and a playerId, return the players hand if the playerId exists.
getHand :: Game -> Int -> Maybe Hand
getHand game pId = view hand <$> game ^. players ^? ix pId

toHand :: [Int] -> Hand
toHand = foldr (\n -> M.insertWith (+) n 1) M.empty

displayHand :: Hand -> String
displayHand h = M.foldrWithKey f "" h
  where
    f k a b = replicate a (head $ show k) ++ b

-- | Get the playerId of the bidder and his bid.
getBid :: Game -> Maybe (Int, Bid)
getBid game = ( , game ^. bid) <$> game ^. bidder

newGame :: Game
newGame = Game 0 [] Nothing (Bid minBound 0) 0 Nothing False False

addPlayer :: Game -> Int -> Game
addPlayer game pId = game & numOfPlayers +~ 1
                             & players <>~ [player]
  where
    player = Player pId "" M.empty 0

dealHands :: Game -> [[Int]] -> Game
dealHands game cs = game & players %~ setHands (toHand <$> cs)
  where
    setHands hs ps =  zipWith (set hand) hs ps

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

legal :: Game -> Action -> Bool
legal game action = case action of
  SetName _ -> not (game ^. inProgress)
  Deal      -> not (game ^. inProgress)
  -- You can't raise a rebid os if you are the bidder and rebid is True
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
--   (score for non-bidders, score for bidder).
value :: Game -> (Int, Int)
value game = (factor, factor * (numPlayers - 1))
  where
    factor     = sixes * mult
    Bid c n    = game ^. bid
    mult       = if n < numPlayers + 3 then 1 else 2 + (n - numPlayers - 3) `div` 2
    sixes      = if c == 6 then 2 else 1
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
