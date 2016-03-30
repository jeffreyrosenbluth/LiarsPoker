{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module WSapi where

import LiarsPoker

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad (replicateM, forever, forM_)
import           Control.Monad.Random
import           Data.Char (isDigit, digitToInt)
import           Data.List.Split  (chunksOf)
import           Data.FileEmbed (embedDir)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Network.Wai
import qualified Network.Wai.Application.Static as Static

type ServerState = (Game, StdGen, Clients)
type Clients     = [WS.Connection]

parseMessage :: Text -> Action
parseMessage t
  | "name " `T.isPrefixOf` t = SetName $ T.drop 5 t
  | "deal" == t = Deal
  | "bid " `T.isPrefixOf` t = do
      let r = do
                (a1, rest) <- uncons $ T.drop 4 t
                (a2, _)    <- uncons $ T.drop 1 rest
                return (a1, a2)
      case r of
        Just (d1, d2) -> if isDigit d1 && isDigit d2
                           then Raise (Bid (digitToInt d2) (digitToInt d1))
                           else Invalid "Invalid raise must take integers."
        Nothing       -> Invalid "Invalid raise"
  | "challenge" == t = Challenge
  | "count" == t = Count
  | "say " `T.isPrefixOf` t = Say $ T.drop 4 t
  | otherwise = Invalid "Invalid message."

-- | Version of sendTextData specialize to Text input.
sendText :: WS.Connection -> Text -> IO ()
sendText = WS.sendTextData

broadcast' :: [Text] -> Clients -> IO ()
broadcast' msgs clients =
  forM_ (zip msgs clients) $ \(msg, conn) -> WS.sendTextData conn msg

broadcast :: Text -> Clients -> IO ()
broadcast msg clients = forM_ clients $ \conn -> WS.sendTextData conn msg

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "./static")

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  getName state conn

getName :: MVar ServerState -> WS.Connection -> IO ()
getName state conn = do
  (g, r, cs) <- takeMVar state
  sendText conn "Please set a user name."
  msg <- WS.receiveData conn
  let pId = g ^. numOfPlayers
      action = parseMessage msg
  case action of
    SetName nm -> do
      let g' = addPlayer g pId nm
      putMVar state (g', r, cs ++ [conn])
      sendText conn nm
      handle conn state pId
    otherwise -> do
      sendText conn "Must set a user name to play"
      putMVar state (g, r, cs)
      getName state conn

handle :: WS.Connection -> MVar ServerState -> Int -> IO ()
handle conn state pId = forever $ do
  msg <- WS.receiveData conn
  let action = parseMessage msg
  case action of
    Deal -> do
      (g, r, cs) <- readMVar state
      if legal g action
        then do
          let (cards, r') = runRand (replicateM (g ^. numOfPlayers * cardsPerHand)
                       $ getRandomR (0, 9)) r
              g' = resetGame $ dealHands g (chunksOf cardsPerHand cards)
              hands = map (T.pack . displayHand . view hand) $ g' ^. players
          swapMVar state (g', r', cs)
          broadcast' hands cs
        else
          sendText conn "Cannot deal a game in progress."
    Raise b -> do
      (g, r, cs) <- readMVar state
      if legal g action && g ^. turn == pId
        then do
          let g' = mkBid g b
          swapMVar state (g', r, cs)
          broadcast (getBidderName g') cs
          broadcast (T.pack . show $ g' ^. bid) cs
        else sendText conn "Illegal raise."
    Challenge -> do
      (g, r, cs) <- readMVar state
      if legal g action && g ^. turn == pId
        then do
          let g' = nextPlayer g
          swapMVar state (g', r, cs)
          broadcast (getBidderName g') cs
        else
          sendText conn "Illegal Challenge."
    Count -> do
      (g, r, cs) <- readMVar state
      if legal g action && g ^. turn == pId
        then do
          let cnt = count g (g ^. bid . bidCard)
              result = g ^. bid . bidQuant <= cnt || cnt == 0
              g' = scoreGame $ g & won .~ Just result & inProgress .~ False
              sc = map (T.pack . show . view score) $ g' ^. players
          swapMVar state (g', r, cs)
          broadcast' sc cs
        else
          sendText conn "Illgal Count."
    Say t -> do
      (_, _, cs) <- readMVar state
      broadcast t cs
    Invalid t -> do
      (_, _, cs) <- readMVar state
      broadcast t cs
