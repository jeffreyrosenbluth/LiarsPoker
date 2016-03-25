{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WSapi where

import LiarsPoker

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad (replicateM)
import           Control.Monad.Random
import           Data.List.Split  (chunksOf)
import           Data.Maybe
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.FileEmbed (embedDir)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import           System.Random


type PlayerDict = IntMap WS.Connection

getConn :: PlayerDict -> Int -> WS.Connection
getConn dict pId = fromJust $ IntMap.lookup pId dict

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")

application :: MVar (Game, StdGen) -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  msg  <- WS.receiveData conn
  case msg of
    _ | (T.isPrefixOf "test" msg) -> WS.sendTextData conn ("Can you hear me now!"  :: Text)
