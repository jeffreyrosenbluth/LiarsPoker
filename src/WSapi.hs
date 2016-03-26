{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WSapi where

import LiarsPoker

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad (replicateM, forever)
import           Control.Monad.Random
import           Data.Monoid ((<>))
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
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "./static")

application :: MVar (Game, StdGen) -> WS.ServerApp
application state pending = do
  (g, r) <- readMVar state
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  let pId = g ^. numOfPlayers
      newG = addPlayer g (pId) (T.unpack "")
      s  = T.pack $ newG ^. players . (singular (ix pId)) . name
  swapMVar state (newG, r)
  WS.sendTextData conn (s <> " " <> (T.pack . show $ pId))
  handle conn state pId

handle :: WS.Connection -> MVar (Game, StdGen) -> Int -> IO ()
handle conn state pId = forever $ do
  msg <- WS.receiveData conn :: IO (Text)
  branch conn msg
  where
    branch c m
      | "name " `T.isPrefixOf` m = do
          (g, r) <- readMVar state
          let nm = T.drop 5 m
              newG = g & players . (singular (ix pId) . name) .~ (T.unpack nm)
          swapMVar state (newG, r)
          WS.sendTextData c nm
      | "deal" == m = do
          (g, r) <- readMVar state
          if g ^. inProgress
            then
              WS.sendTextData c $ T.pack "Cannot deal a game in progress."
            else
              do
              let (cs, r') = runRand (replicateM (g ^. numOfPlayers * cardsPerHand)
                           $ getRandomR (0, 9)) r
                  newG = dealHands g (chunksOf cardsPerHand cs) & inProgress .~ True
                  s  = T.pack . show $ newG ^. players . (singular (ix pId)) . hand
              swapMVar state (newG, r')
              WS.sendTextData c s
      | "hand" == m = do
          (g, _) <- readMVar state
          let s = T.pack . show $ g ^. players . (singular (ix pId)) . hand
          WS.sendTextData c s
      | otherwise = WS.sendTextData c ("You said: " <> m)
