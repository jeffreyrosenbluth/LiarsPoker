{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WSapi where

import LiarsPoker

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad (replicateM, forever, forM_)
import           Control.Monad.Random
import           Data.Char (digitToInt)
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

type Clients = [WS.Connection]

-- | Version of sendTextData specialize to Text input.
sendText :: WS.Connection -> Text -> IO ()
sendText = WS.sendTextData

getConn :: PlayerDict -> Int -> WS.Connection
getConn dict pId = fromJust $ IntMap.lookup pId dict

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "./static")

broadcast :: [Text] -> Clients -> IO ()
broadcast msgs clients =
  forM_ (zip msgs clients) $ \(msg, conn) -> WS.sendTextData conn msg

application :: MVar (Game, StdGen, Clients) -> WS.ServerApp
application state pending = do
  (g, r, cs) <- readMVar state
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  let pId = g ^. numOfPlayers
      newG = addPlayer g (pId)
  swapMVar state (newG, r, conn : cs)
  sendText conn "You have successfuly joined the game."
  handle conn state pId

handle :: WS.Connection -> MVar (Game, StdGen, Clients) -> Int -> IO ()
handle conn state pId = forever $ do
  msg <- WS.receiveData conn
  branch conn msg
  where
    branch c m
      | "name " `T.isPrefixOf` m = do
          (g, r, cs) <- readMVar state
          let nm = T.drop 5 m
              newG = g & players . (singular (ix pId) . name) .~ nm
          swapMVar state (newG, r, cs)
          sendText c nm
      | "deal" == m = do
          (g, r, cs) <- readMVar state
          if g ^. inProgress
            then
              sendText c "Cannot deal a game in progress."
            else do
              let (cards, r') = runRand (replicateM (g ^. numOfPlayers * cardsPerHand)
                           $ getRandomR (0, 9)) r
                  newG = dealHands g (chunksOf cardsPerHand cards) & inProgress .~ True
                  hands = map (T.pack . displayHand . view hand) $ newG ^. players
              swapMVar state (newG, r', cs)
              broadcast hands cs
      | "challenge" == m = do
          (g, r, cs) <- readMVar state
          if g ^. turn == pId && legal g Challenge
            then do
              let newG = nextPlayer g
              broadcast (repeat . T.pack . show $ g ^. bid) cs
            else
              sendText c "Illegal Challenge."
      | "bid " `T.isPrefixOf` m = do
          let q = do
                    (a1, rest) <- uncons $ T.drop 4 m
                    (a2, _)    <- uncons $ T.drop 1 rest
                    return $ Bid (digitToInt a1) (digitToInt a2)
          case q of
            Nothing -> sendText c ("Invalid Bid")
            Just b  -> do
              (g, r, cs) <- readMVar state
              let newG = mkBid g b
              swapMVar state (newG, r, cs)
              broadcast (repeat . T.pack . show $ newG ^. bid) cs
      | otherwise = sendText c ("You said: " <> m)
