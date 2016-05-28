{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module WSapi where

import           LiarsPoker

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad                  (forM_, forever, replicateM)
import           Control.Monad.Random
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as LB
import           Data.Char                      (digitToInt, isDigit)
import           Data.FileEmbed                 (embedDir)
import           Data.List.Split                (chunksOf)
import           Data.Maybe
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Read                 (decimal)
import           GHC.Generics
import qualified Network.Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.WebSockets             as WS

type ServerState = (Game, StdGen, Clients)
type Clients     = [WS.Connection]

data PlayerPublic = PlayerPublic
  { _playerIdOP :: Int
  , _nameOP     :: Text
  , _scoreOP    :: Int
  } deriving (Show, Generic)

instance ToJSON PlayerPublic
instance FromJSON PlayerPublic

makeLenses ''PlayerPublic

data ClinetMsg = ClinetMsg
  { _playersMsg   :: [PlayerPublic]
  , _bidderMsg    :: Text
  , _bidQuantMsg  :: Int
  , _bidCardMsg   :: Int
  , _turnMsg      :: Text
  , _baseStakeMsg :: Int
  , _myNameMsg    :: Text
  , _myHandMsg    :: Text
  } deriving (Show, Generic)

instance ToJSON ClinetMsg
instance FromJSON ClinetMsg

makeLenses ''ClinetMsg

playerPublics :: Game -> [PlayerPublic]
playerPublics g = map playerPublic (g ^.players)
  where
    playerPublic p = PlayerPublic (p ^. playerId) (p ^. name) (p ^. score)

clinetMsgs :: Game -> [ClinetMsg]
clinetMsgs g = map (\p -> ClinetMsg
  (playerPublics g)
  (maybe "" (\i -> ((g ^. players) !! i) ^. name) (g ^. bidder))
  (g ^. bid . bidQuant)
  (g ^. bid . bidCard)
  (map fst playerPrivates !! (g ^. turn))
  (g ^. baseStake)
  (fst p)
  (T.pack . displayHand $ snd p)) playerPrivates
  where
    playerPrivates = map (\p -> (p ^. name, p ^. hand)) (g ^. players)

parseMessage :: Text -> Action
parseMessage t
  | "name " `T.isPrefixOf` t = SetName $ T.drop 5 t
  | "deal" == t = Deal
  | "bid " `T.isPrefixOf` t = do
      let r = do
                (b1, t1) <- decimal $ T.drop 4 t
                (b2, _) <- decimal $ T.drop 1 t1
                return (b2, b1)
      case r of
        Right (d1, d2) -> Raise (Bid d1 d2)
        Left e         -> Invalid (T.pack e)
  | "challenge" == t = Challenge
  | "count" == t = Count
  | "say " `T.isPrefixOf` t = Say $ T.drop 4 t
  | otherwise = Invalid "Invalid message."

-- | Version of sendTextData specialize to Text input.
sendText :: WS.Connection -> Text -> IO ()
sendText = WS.sendTextData

broadcast :: [Text] -> Clients -> IO ()
broadcast msgs clients =
  forM_ (zip msgs clients) $ \(msg, conn) -> WS.sendTextData conn msg

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
      sendText conn (last $ map (T.pack . LB.unpack . encode) $ clinetMsgs g')
      handle conn state pId
    _ -> do
      sendText conn "Must set a user name to play"
      putMVar state (g, r, cs)
      getName state conn

deal :: WS.Connection -> MVar ServerState -> IO ()
deal conn state = do
  (g, r, cs) <- readMVar state
  if legal g Deal
    then do
      let (cards, r') = runRand (replicateM (g ^. numOfPlayers * cardsPerHand)
                   $ getRandomR (0, 9)) r
          g' = resetGame $ dealHands g (chunksOf cardsPerHand cards)
      swapMVar state (g', r', cs)
      broadcast (map (T.pack . LB.unpack . encode) $ clinetMsgs g') cs
    else
      sendText conn "Cannot deal a game in progress."

handle :: WS.Connection -> MVar ServerState -> Int -> IO ()
handle conn state pId = forever $ do
  msg <- WS.receiveData conn
  let action = parseMessage msg
  case action of
    Deal -> deal conn state
    Raise b -> do
      (g, r, cs) <- readMVar state
      if legal g action && g ^. turn == pId
        then do
          let g' = mkBid g b
          swapMVar state (g', r, cs)
          broadcast (map (T.pack . LB.unpack . encode) $ clinetMsgs g') cs
        else sendText conn "Illegal raise."
    Challenge -> do
      (g, r, cs) <- readMVar state
      if legal g action && g ^. turn == pId
        then do
          let g' = nextPlayer g
          swapMVar state (g', r, cs)
          broadcast (map (T.pack . LB.unpack . encode) $ clinetMsgs g') cs
        else
          sendText conn "Illegal Challenge."
    Count -> do
      (g, r, cs) <- readMVar state
      if legal g action && g ^. turn == pId
        then do
          let cnt = count g (g ^. bid . bidCard)
              result = g ^. bid . bidQuant <= cnt || cnt == 0
              g' = scoreGame $ g & won .~ Just result & inProgress .~ False
          swapMVar state (g', r, cs)
          broadcast (map (T.pack . LB.unpack . encode) $ clinetMsgs g') cs
          deal conn state
        else
          sendText conn "Illgal Count."
    _ -> return ()
    -- Say t -> do
    --   (_, _, cs) <- readMVar state
    --   broadcast t cs
    -- Invalid t -> do
    --   (_, _, cs) <- readMVar state
    --   broadcast t cs
