{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LiarsPoker
  ( Card, cardsPerHand
  , Bid(..), bidCard, bidQuant
  , Player(..), name, hand, score, playerId
  , Game(..), numOfPlayers, players, bidder, bid, turn, won, rebid
  , Action(..), _Raise, _Challenge, _Count

  , newGame
  , resetGame
  , addPlayer
  , dealHands
  , toHand
  , displayHand
  , getHand
  , getBidderName
  , mkBid
  , nextPlayer
  , count
  , legal
  , scoreGame
  , inProgress
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
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
  Bid c1 q1 <= Bid c2 q2 = (q1, f c1) <= (q2, f c2)
    where
      f j = if j == 0 then 10 else j

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
  , _baseStake    :: Int
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

-- | Total number of Card in the game.
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

getBidderName :: Game -> Text
getBidderName g = fromMaybe "" $ fmap (\i -> view (ix i . name) ps) b
  where
    b = g ^. bidder
    ps = g ^. players

newGame :: Game
newGame = Game 0 [] Nothing (Bid minBound 0) 0 Nothing False False 1

resetGame :: Game -> Game
resetGame g = g & bidder .~ Nothing
                & bid .~ Bid minBound 0
                & turn .~ fromMaybe 0 (g ^. bidder)
                & won .~ Nothing
                & rebid .~ False
                & inProgress .~ True

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
--   the bonus multiplier. Both the n+3 rule and the Sixes rule.
bonus :: Game -> Int
bonus game = sixes * mult
  where
    Bid c n    = game ^. bid
    mult       = if n < numPlayers + 3 then 1 else 2 + (n - numPlayers - 3) `div` 2
    sixes      = if c == 6 then 2 else 1
    numPlayers = game ^. numOfPlayers

-- | The hero bump is 1 if the bidder wins with none.
hero :: Game -> Int
hero game = if q == 0 && count game bc > 0 then 1 else 0
  where
    Just bdr = game ^. bidder
    q = fromMaybe 0 $ M.lookup bc (game ^. players . singular (ix bdr) . hand)
    bc = game ^. bid ^. bidCard


-- | Score the game and set the new 'baseStake' in accordance with Progressive
--   Stakes.
scoreGame :: Game -> Game
scoreGame game = game & players .~ (reScore <$> [0..(game ^. numOfPlayers - 1)])
                      & baseStake .~ (if h == 1 then 2 else bns)
  where
    reScore p
      | game ^. bidder == Just p =
          (over score (+ x)) (game ^. players . singular (ix p))
      | otherwise =
          (over score (+ a)) (game ^. players . singular (ix p))
    (a , x)   = maybe (0, 0) (\w -> if w
                                      then (-winStake, winB)
                                      else (lossStake, -lossB))
                             (game ^. won)
    cnt       = count game (game ^. bid . bidCard)
    -- The n+3 rule.
    bns       = bonus game
    -- The skunk rule.
    mult      = if cnt == 0 then max 1 (2 * game ^. numOfPlayers - 6) else bns
    -- The hero bump.
    h         = hero game
    -- The score for non bidders.
    winStake  = (mult + h) * game ^. baseStake
    lossStake = mult * game ^. baseStake
    -- The score for the bidder
    winB      = winStake * (game ^. numOfPlayers - 1)
    lossB     = lossStake * (game ^. numOfPlayers - 1)
