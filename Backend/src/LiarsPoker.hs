{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module LiarsPoker where

import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.List    (intersperse, sortOn)
import           Data.Map     (Map)
import qualified Data.Map     as M
import           Data.Maybe
import           Data.Text    (Text)
import           Data.Vector  (Vector, (!?))
import qualified Data.Vector  as V
import           GHC.Generics

type Card  = Int
type Hand  = Map Card Int
type Hands = Vector Hand

instance ToJSON Hand where
  toJSON = toJSON . M.toList

instance FromJSON Hand where
  parseJSON = fmap M.fromList . parseJSON

data Bid = Bid
  { _bidCard  :: !Card
  , _bidQuant :: !Int
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
  { _playerId :: !Int
  , _name     :: !Text
  , _score    :: !Int
  } deriving (Show, Eq, Generic)
makeLenses ''Player

instance ToJSON Player
instance FromJSON Player

data Game = Game
  { _players    :: !(Vector Player)
  , _bidder     :: !(Maybe Int)  -- ^ playerId
  , _bid        :: !Bid
  , _turn       :: !Int        -- ^ playerId
  , _won        :: !(Maybe Bool)
  , _rebid      :: !Bool
  , _inProgress :: !Bool
  , _baseStake  :: !Int
  } deriving (Show, Generic)
makeLenses ''Game

instance ToJSON Game
instance FromJSON Game

data Action
  = SetName !Text
  | Deal
  | Raise !Bid
  | Challenge
  | Count
  | Say !Text
  | Invalid !Text
  deriving (Show, Generic)
makePrisms ''Action

instance ToJSON Action
instance FromJSON Action

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

newGame :: Game
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
  SetName _ -> not (game ^. inProgress)
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
