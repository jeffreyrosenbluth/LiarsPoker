{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

----------------------------------------------------------
-- |
-- Types for LiarsPoker multiplayer game
-- (c) 2016 Jeffrey Rosenbluth
----------------------------------------------------------

module Types where

import           Control.Lens            hiding ((.=))
import           Data.Aeson
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Text               (Text)
import           Data.Vector             (Vector)
import           GHC.Generics
import qualified Network.WebSockets      as WS
import           System.Random           (StdGen)

--------------------------------------------------------------------------------
-- For game logic.
--------------------------------------------------------------------------------

type Rank  = Int
type Hand  = Map Rank Int
type Hands = Vector Hand

instance ToJSON Hand where
  toJSON = toJSON . M.toList

instance FromJSON Hand where
  parseJSON = fmap M.fromList . parseJSON

data Bid = Bid
  { _bidRank  :: !Rank
  , _bidQuant :: !Int
  } deriving (Eq, Generic, Show)

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
  , _bidder     :: !(Maybe Int) -- ^ playerId
  , _bid        :: !Bid
  , _turn       :: !Int         -- ^ playerId
  , _won        :: !(Maybe Bool)
  , _rebid      :: !Bool
  , _inProgress :: !Bool
  , _baseStake  :: !Int
  , _gameId     :: !Int
  , _numPlyrs   :: !Int
  } deriving (Show, Generic)
makeLenses ''Game

instance ToJSON Game
instance FromJSON Game

data Action
  = Join !Text !Int
  | New !Text !Int
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

--------------------------------------------------------------------------------
-- For web api.
--------------------------------------------------------------------------------

type Clients     = [WS.Connection]

data GameState = GameState
  { _stGame   :: !Game
  , _stHands  :: !Hands
  , _stStdGen :: StdGen
  }

makeLenses ''GameState

data BtnFlags = BtnFlags
  { _bfRaise     :: !Bool
  , _bfChallenge :: !Bool
  , _bfCount     :: !Bool
  } deriving (Show, Generic)

instance ToJSON BtnFlags
instance FromJSON BtnFlags

makeLenses ''BtnFlags

data PrevGame = PrevGame
  { _pgBidder :: !Text
  , _pgBid    :: !Bid
  , _pgCount  :: !Int
  , _pgMe     :: !(Vector Int)
  } deriving (Show, Generic)

instance ToJSON PrevGame
instance FromJSON PrevGame

makeLenses ''PrevGame

data ClientMsg = ClientMsg
  { _cmGame     :: !Game
  , _cmHand     :: !Text
  , _cmError    :: !Text
  , _cmMultiple :: !Int
  , _cmButtons  :: !BtnFlags
  , _cmName     :: !Text
  , _cmPrevGame :: !PrevGame
  , _cmPlyrId   :: !Int
  } deriving (Show, Generic)

instance ToJSON ClientMsg
instance FromJSON ClientMsg

makeLenses ''ClientMsg
