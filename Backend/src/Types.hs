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
import           Data.Map                (Map, insertWith, foldrWithKey)
import qualified Data.Map                as M
import           Data.Text               (Text)
import           Data.Vector             (Vector)
import           GHC.Generics

type Rank  = Int
type Hand  = Map Rank Int
type Hands = Vector Hand

instance ToJSON Hand where
  toJSON = toJSON . M.toList

hand :: Iso' Hand [Int]
hand = iso fromHand toHand
  where
    toHand = foldr (\n -> insertWith (+) n 1) mempty
    fromHand = foldrWithKey (\k a b -> replicate a k ++ b) []

data Bid = Bid
  { _bidRank  :: !Rank
  , _bidQuant :: !Int
  } deriving (Eq, Generic, Show)

instance ToJSON Bid

instance Ord Bid where
  Bid c1 q1 <= Bid c2 q2 = (q1, f c1) <= (q2, f c2)
    where
      f j = if j == 0 then 10 else j

data Flags = Flags
  { _raiseFlag :: !Bool
  , _chalFlag  :: !Bool
  , _countFlag :: !Bool
  , _dealFlag  :: !Bool
  } deriving (Show, Generic, Eq)

instance ToJSON Flags

data Player = Player
  { _playerId :: !Int
  , _name     :: !Text
  , _score    :: !Int
  , _flags    :: !Flags
  , _bot      :: Maybe (Game (Vector Hand) -> Game (Vector Hand))
  } deriving (Generic)

instance ToJSON Player where
  toJSON p = object
    [ "_playerId" .= _playerId p
    , "_name"     .= _name p
    , "_score"    .= _score p
    , "_flags"    .= _flags p
    ]

data Game a = Game
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
  , _variant    :: a
  , _multiple   :: !Int
  } deriving (Generic)

instance ToJSON (Game (Int, Text))
instance ToJSON (Game (Vector Hand))

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

instance ToJSON Action

makeLenses ''Bid
makeLenses ''Flags
makeLenses ''Player
makeLenses ''Game
makePrisms ''Action
