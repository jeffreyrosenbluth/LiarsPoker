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
import           Data.FileEmbed                 (embedDir)
import           Data.List.Split                (chunksOf)
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
  { _pbPlayerId :: Int
  , _pbName     :: Text
  , _pbScore    :: Int
  } deriving (Show, Generic)

instance ToJSON PlayerPublic
instance FromJSON PlayerPublic

makeLenses ''PlayerPublic

data ClientMsg = ClientMsg
  { _cmPlayers   :: [PlayerPublic]
  , _cmBidder    :: Text
  , _cmBidQuant  :: Int
  , _cmBidCard   :: Int
  , _cmTurn      :: Text
  , _cmBaseStake :: Int
  , _cmMultiple  :: Int
  , _cmMyName    :: Text
  , _cmMyHand    :: Text
  , _cmError     :: Text
  , _cmRaiseBtn  :: Bool
  , _cmChalBtn   :: Bool
  , _cmCountBtn  :: Bool
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
clientMsgs g = map cm (playerIds g)
  where
    cm p =  ClientMsg
      { _cmPlayers = playerPublics g
      , _cmBidder = getBidderName g
      , _cmBidQuant = g ^. bid . bidQuant
      , _cmBidCard = g ^. bid . bidCard
      , _cmTurn = getTurnName g
      , _cmBaseStake = g ^. baseStake
      , _cmMultiple = bonus g
      , _cmMyName = getPlayerName g p
      , _cmMyHand = T.pack . displayHand $ getHand g p
      , _cmError = ""
      , _cmRaiseBtn = False
      , _cmChalBtn = False
      , _cmCountBtn = False
      }

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
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "../static")

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  getName state conn

getName :: MVar ServerState -> WS.Connection -> IO ()
getName state conn = do
  (g, r, cs) <- takeMVar state
  sendText conn ":signin"
  msg <- WS.receiveData conn
  let pId    = numOfPlayers g
      action = parseMessage msg
  case action of
    SetName nm -> do
      let g' = addPlayer g pId nm
          cs' = cs ++ [conn]
      putMVar state (g', r, cs')
      broadcast cs' (map (T.pack . LB.unpack . encode) $ clientMsgs g')
      handle conn state pId
    _ -> do
      sendText conn ":signin"
      putMVar state (g, r, cs)
      getName state conn

handle :: WS.Connection -> MVar ServerState -> Int -> IO ()
handle conn state pId = forever $ do
  msg        <- WS.receiveData conn
  (g, r, cs) <- readMVar state
  let action         = parseMessage msg
      ((g', cm), r') = case action of
                         Deal      -> deal g r
                         Raise b   -> (raise g pId b, r)
                         Challenge -> (challenge g pId, r)
                         Count     -> count g r pId
                         Say m     -> error "Not implemented yet"
                         Invalid m -> error (T.unpack m)
  swapMVar state (g', r', cs)
  broadcast cs (map (T.pack . LB.unpack . encode) cm)

deal :: Game -> StdGen -> ((Game, [ClientMsg]), StdGen)
deal g r
  | legal g Deal = ((g', cm), r'')
  | otherwise    = ((g, cm'), r)
    where
      (cards, r') = runRand (replicateM (numOfPlayers g * cardsPerHand)
                  $ getRandomR (0, 9)) r
      (f, r'')    = runRand (getRandomR (0, numOfPlayers g - 1)) r'
      g'          = resetGame f $ dealHands g (chunksOf cardsPerHand cards)
      cm          = clientMsgs g'
                  & traverse . cmError .~ ""
                  & singular (ix (g' ^. turn)) . cmRaiseBtn .~ True
      cm'         = clientMsgs g
                  & traverse . cmError .~ "Cannot deal a game in progress"

updateClientMsgs :: [ClientMsg] -> Game -> Text -> [ClientMsg]
updateClientMsgs cs g err  =
  cs & traverse . cmError .~ err
     & singular (ix (g ^. turn)) . cmRaiseBtn .~ True
     & singular (ix (g ^. turn)) . cmChalBtn  .~ ((Just $ g ^. turn) /= g ^. bidder)
     & singular (ix (g ^. turn)) . cmCountBtn .~ ((Just $ g ^. turn) == g ^. bidder)

raise :: Game -> Int -> Bid -> (Game, [ClientMsg])
raise g pId b
  | legal g (Raise b) && g ^. turn == pId = (g', cm)
  | otherwise = (g, cm')
    where
      g'  = mkBid g b
      cm  = updateClientMsgs (clientMsgs g') g' ""
      cm' = updateClientMsgs (clientMsgs g) g e
      e   = "Either your bid is too low or you are trying to raise a re-bid."

challenge :: Game -> Int -> (Game, [ClientMsg])
challenge g pId
  | legal g Challenge && g ^. turn == pId = (g', cm)
  | otherwise = (g, updateClientMsgs (clientMsgs g) g "Illegal Challenge")
      where
        g' = nextPlayer g
        cm = updateClientMsgs (clientMsgs g') g' ""

count :: Game -> StdGen -> Int -> ((Game, [ClientMsg]), StdGen)
count g r pId
  | legal g Count && g ^. turn == pId = deal g' r
  | otherwise = ((g, updateClientMsgs (clientMsgs g) g "Illegal Count"), r)
      where
        cnt    = countCard g (g ^. bid . bidCard)
        result = g ^. bid . bidQuant <= cnt || cnt == 0
        g'     = scoreGame $ g & won .~ Just result & inProgress .~ False
