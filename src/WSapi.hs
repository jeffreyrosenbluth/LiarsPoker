{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module WSapi where

import           LiarsPoker

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad                  (forever, replicateM, zipWithM_)
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

data ClientMsg = ClientMsg
  { _playersMsg   :: [PlayerPublic]
  , _bidderMsg    :: Text
  , _bidQuantMsg  :: Int
  , _bidCardMsg   :: Int
  , _turnMsg      :: Text
  , _baseStakeMsg :: Int
  , _multipleMsg  :: Int
  , _myNameMsg    :: Text
  , _myHandMsg    :: Text
  , _errorMsg     :: Text
  , _bidBtnsMsg  :: Bool
  , _countBtnMsg  :: Bool
  } deriving (Show, Generic)

instance ToJSON ClientMsg
instance FromJSON ClientMsg

makeLenses ''ClientMsg

playerIds :: Game -> [Int]
playerIds g = [0..(numOfPlayers g - 1)]

playerPublics :: Game -> [PlayerPublic]
playerPublics g = map playerPublic (g ^.players)
  where
    playerPublic p = PlayerPublic (p ^. playerId) (p ^. name) (p ^. score)

clientMsgs :: Game -> [ClientMsg]
clientMsgs g = map ( \p -> ClientMsg
  (playerPublics g)
  (getBidderName g)
  (g ^. bid . bidQuant)
  (g ^. bid . bidCard)
  (getTurnName g)
  (g ^. baseStake)
  (bonus g)
  (getPlayerName g p)
  (T.pack . displayHand $ getHand g p)
  ""
  False
  False )
    (playerIds g)

parseMessage :: Text -> Action
parseMessage t
  | "name " `T.isPrefixOf` t = SetName $ T.drop 5 t
  | "deal" == t = Deal
  | "bid " `T.isPrefixOf` t = do
      let r = do
                (b1, t') <- decimal $ T.drop 4 t
                (b2, _ ) <- decimal $ T.drop 1 t'
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

broadcast :: Clients -> [Text] -> IO ()
broadcast = zipWithM_ WS.sendTextData

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
  let pId = numOfPlayers g
      action = parseMessage msg
  case action of
    SetName nm -> do
      let g' = addPlayer g pId nm
          cs' = cs ++ [conn]
      putMVar state (g', r, cs')
      broadcast cs' (map (T.pack . LB.unpack . encode) $ clientMsgs g')
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
      let (cards, r') = runRand (replicateM (numOfPlayers g * cardsPerHand)
                   $ getRandomR (0, 9)) r
          g' = resetGame $ dealHands g (chunksOf cardsPerHand cards)
          cm = clientMsgs g' & traverse . errorMsg .~ ""
                             & singular (ix (g' ^. turn)) . bidBtnsMsg .~ True
      swapMVar state (g', r', cs)
      broadcast cs (map (T.pack . LB.unpack . encode) cm)
    else do
      let cm = clientMsgs g & traverse . errorMsg
                           .~ "Cannot deal a game in progress"
      broadcast cs (map (T.pack . LB.unpack . encode) cm)

raise :: WS.Connection -> MVar ServerState -> Int -> Bid -> IO ()
raise conn state pId b = do
  (g, r, cs) <- readMVar state
  if legal g (Raise b) && g ^. turn == pId
    then do
      let g' = mkBid g b
          cm = clientMsgs g' & traverse . errorMsg .~ ""
                             & singular (ix (g' ^. turn)) . bidBtnsMsg .~ True
      swapMVar state (g', r, cs)
      broadcast cs (map (T.pack . LB.unpack . encode) cm)
    else do
      let cm = clientMsgs g & traverse . errorMsg
                           .~ "Illegal Raise"
      broadcast cs (map (T.pack . LB.unpack . encode) cm)

challenge :: WS.Connection -> MVar ServerState -> Int -> IO ()
challenge conn state pId = do
  (g, r, cs) <- readMVar state
  if legal g Challenge && g ^. turn == pId
    then do
      let g' = nextPlayer g
          cm = clientMsgs g' & traverse . errorMsg .~ ""
                             & singular (ix (g' ^. turn)) . bidBtnsMsg .~ True
                             & singular (ix (g' ^. turn)) . countBtnMsg .~
                                 (g' ^. turn == fromMaybe (-1) (g' ^. bidder))
      swapMVar state (g', r, cs)
      broadcast cs (map (T.pack . LB.unpack . encode) cm)
    else do
      let cm = clientMsgs g & traverse . errorMsg
                           .~ "Illegal Challenge"
      broadcast cs (map (T.pack . LB.unpack . encode) cm)


count' :: WS.Connection -> MVar ServerState -> Int -> IO ()
count' conn state pId = do
  (g, r, cs) <- readMVar state
  if legal g Count && g ^. turn == pId
    then do
      let cnt = count g (g ^. bid . bidCard)
          result = g ^. bid . bidQuant <= cnt || cnt == 0
          g' = scoreGame $ g & won .~ Just result & inProgress .~ False
          cm = clientMsgs g' & traverse . errorMsg .~ ""
      swapMVar state (g', r, cs)
      broadcast cs (map (T.pack . LB.unpack . encode) cm)
      deal conn state
    else do
      let cm = clientMsgs g & traverse . errorMsg
                           .~ "Illegal Count"
      broadcast cs (map (T.pack . LB.unpack . encode) cm)

handle :: WS.Connection -> MVar ServerState -> Int -> IO ()
handle conn state pId = forever $ do
  msg <- WS.receiveData conn
  let action = parseMessage msg
  case action of
    Deal -> deal conn state
    Raise b -> raise conn state pId b
    Challenge -> challenge conn state pId
    Count -> count' conn state pId
    _ -> return ()
