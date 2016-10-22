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
makeLenses ''Bid

instance ToJSON Bid
instance FromJSON Bid

instance Ord Bid where
  Bid c1 q1 <= Bid c2 q2 = (q1, f c1) <= (q2, f c2)
    where
      f j = if j == 0 then 10 else j

data Flags = Flags
  { _raiseFlag :: !Bool
  , _chalFlag  :: !Bool
  , _countFlag :: !Bool
  } deriving (Show, Generic, Eq)
makeLenses ''Flags

instance ToJSON Flags
instance FromJSON Flags

data Player = Player
  { _playerId :: !Int
  , _name     :: !Text
  , _score    :: !Int
  , _flags    :: !Flags
  } deriving (Show, Eq, Generic)
makeLenses ''Player

instance ToJSON Player
instance FromJSON Player

data Game f = Game
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
  , _hands      :: f Hand
  , _multiple   :: !Int
  } deriving (Generic)
makeLenses ''Game

instance ToJSON (Game Identity)
instance FromJSON (Game Identity)

instance ToJSON (Game Vector)
instance FromJSON (Game Vector)

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
  { _stGame   :: Game Vector
  , _stStdGen :: StdGen
  }

makeLenses ''GameState
