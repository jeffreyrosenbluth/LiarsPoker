{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WSapi where

import LiarsPoker

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad (replicateM, forever, forM_)
import           Control.Monad.Random
import           Data.Char (isDigit, digitToInt)
import           Data.Monoid ((<>))
import           Data.List.Split  (chunksOf)
import           Data.Maybe
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


type ServerState = (Game, StdGen, Clients)
type Clients     = [WS.Connection]

parseMessage :: Text -> Action
parseMessage t
  | "name " `T.isPrefixOf` t = SetName $ T.drop 5 t
  | "deal" == t = Deal
  | "raise " `T.isPrefixOf` t = do
      let r = do
                (a1, rest) <- uncons $ T.drop 6 t
                (a2, _)    <- uncons $ T.drop 1 rest
                return (a1, a2)
      case r of
        Just (d1, d2) -> if isDigit d1 && isDigit d2
                           then Raise (Bid (digitToInt d1) (digitToInt d2))
                           else Invalid "Invalid raise must take integers."
        Nothing       -> Invalid "Invalid raise"
  | "challenge" == t = Challenge
  | "count" == t = Count
  | "say " `T.isPrefixOf` t = Say $ T.drop 4 t
  | otherwise = Invalid "Invalid message."

-- | Version of sendTextData specialize to Text input.
sendText :: WS.Connection -> Text -> IO ()
sendText = WS.sendTextData

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "./static")

broadcast :: [Text] -> Clients -> IO ()
broadcast msgs clients =
  forM_ (zip msgs clients) $ \(msg, conn) -> WS.sendTextData conn msg

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  (g, r, cs) <- readMVar state
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  let pId = g ^. numOfPlayers
      g' = addPlayer g (pId)
  swapMVar state (g', r, cs ++ [conn])
  sendText conn "You have successfuly joined the game."
  handle conn state pId

handle :: WS.Connection -> MVar ServerState -> Int -> IO ()
handle conn state pId = forever $ do
  msg <- WS.receiveData conn
  let action = parseMessage msg
  case action of
    SetName nm -> do
      (g, r, cs) <- readMVar state
      if legal g action
        then do
          let g' = g & players . (singular (ix pId) . name) .~ nm
          swapMVar state (g', r, cs)
          sendText conn nm
        else sendText conn "Cannot change name mid game."
    Deal -> do
      (g, r, cs) <- readMVar state
      if legal g action
        then do
          let (cards, r') = runRand (replicateM (g ^. numOfPlayers * cardsPerHand)
                       $ getRandomR (0, 9)) r
              g' = dealHands g (chunksOf cardsPerHand cards)
                 & bidder .~ Nothing
                 & bid .~ Bid minBound 0
                 & turn .~ 0
                 & won .~ Nothing
                 & rebid .~ False
                 & inProgress .~ True
              hands = map (T.pack . displayHand . view hand) $ g' ^. players
          swapMVar state (g', r', cs)
          broadcast hands cs
        else
          sendText conn "Cannot deal a game in progress."
    Raise b -> do
      (g, r, cs) <- readMVar state
      if legal g action && g ^. turn == pId
        then do
          let g' = mkBid g b
          swapMVar state (g', r, cs)
          broadcast (repeat . T.pack . show $ g' ^. bid) cs
        else sendText conn "Illegal raise."
    Challenge -> do
      (g, r, cs) <- readMVar state
      if legal g action && g ^. turn == pId
        then do
          let g' = nextPlayer g
          swapMVar state (g', r, cs)
          broadcast (repeat . T.pack . show $ g ^. bid) cs
        else
          sendText conn "Illegal Challenge."
    Count -> do
      (g, r, cs) <- readMVar state
      if legal g action && g ^. turn == pId
        then do
          let result = g ^. bid . bidQuant <= count g (g ^. bid . bidCard)
              g' = scores $ g & won .~ Just result & inProgress .~ False
              sc = map (T.pack . show . view score) $ g' ^. players
          swapMVar state (g', r, cs)
          broadcast sc cs
        else
          sendText conn "Illgal Count."
    Say t -> do
      (_, _, cs) <- readMVar state
      broadcast (repeat t) cs
    Invalid t -> do
      (_, _, cs) <- readMVar state
      broadcast (repeat t) cs
