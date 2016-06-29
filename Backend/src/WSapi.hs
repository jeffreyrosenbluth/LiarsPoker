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
import           Data.IntMap                    (IntMap, (!), keys, insert)
import qualified Data.IntMap.Strict             as IM
import           Data.List.Split                (chunksOf)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Read                 (decimal)
import qualified Data.Vector                    as V
import           GHC.Generics
import qualified Network.Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.WebSockets             as WS
import           System.Random                  (getStdGen)

type Clients     = [WS.Connection]

data GameState = GameState
  { _stGame   :: Game
  , _stHands  :: Hands
  , _stStdGen :: StdGen
  }

makeLenses ''GameState

type ServerState = (GameState, Clients)

type GameMap = IntMap (MVar ServerState)

data BtnFlags = BtnFlags
  { _bfRaise     :: Bool
  , _bfChallenge :: Bool
  , _bfCount     :: Bool
  } deriving (Show, Generic)

instance ToJSON BtnFlags
instance FromJSON BtnFlags

makeLenses ''BtnFlags

data ClientMsg = ClientMsg
  { _cmGame     :: Game
  , _cmHand     :: Text
  , _cmError    :: Text
  , _cmMultiple :: Int
  , _cmButtons  :: BtnFlags
  , _cmName     :: Text
  } deriving (Show, Generic)

instance ToJSON ClientMsg
instance FromJSON ClientMsg

makeLenses ''ClientMsg

playerIds :: Game -> [Int]
playerIds g = [0..(numOfPlayers g - 1)]

clientMsgs :: Game -> Hands -> [ClientMsg]
clientMsgs g hs = map cm (playerIds g)
  where
    cm p =  ClientMsg
      { _cmGame = g
      , _cmMultiple = bonus g
      , _cmHand = T.pack . displayHand $ getHand hs p
      , _cmError = ""
      , _cmButtons = BtnFlags False False False
      , _cmName = getPlayerName g p
      }

parseMessage :: Text -> Action
parseMessage t
  | "name " `T.isPrefixOf` t = Join (T.drop 5 t) 0
  | "new " `T.isPrefixOf` t = New (T.drop 4 t) 0
  | "deal" == t = Deal
  | "bid " `T.isPrefixOf` t =
      let
        r = do
          (b1, t') <- decimal $ T.drop 4 t
          (b2, _ ) <- decimal $ T.drop 1 t'
          return (b2, b1)
      in
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

-- | Send a list a messages to a list of clients.
broadcast :: Clients -> [Text] -> IO ()
broadcast = zipWithM_ WS.sendTextData

-- | Serialize a list of client messages to a list of JSON text.
encodeCMs :: [ClientMsg] -> [Text]
encodeCMs = map $ T.pack . LB.unpack . encode

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "../Frontend/dist")

application :: MVar GameMap -> WS.ServerApp
application gm pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  singIn gm conn

singIn :: MVar GameMap -> WS.Connection -> IO ()
singIn gmRef conn = do
  sendText conn ":signin"
  msg <- WS.receiveData conn
  case parseMessage msg of
    New nm nPlyrs -> do
      r <- getStdGen
      let g = addPlayer newGame 0 nm
          state = GameState g V.empty r
      gs <- newMVar (state, [conn])
      gm <- takeMVar gmRef
      let key = if null gm then 0 else 1 + (maximum . keys $ gm)
          gm' = insert key gs gm
      putMVar gmRef gm'
      broadcast [conn] (encodeCMs $ clientMsgs g V.empty)
      handle conn gs 0
    Join nm gId -> do
      gm <- readMVar gmRef
      let state = gm ! gId
      (GameState g hs r, cs) <- takeMVar state
      let pId = numOfPlayers g
          g' = addPlayer g pId nm
          cs' = cs ++ [conn]
      putMVar state (GameState g' V.empty r, cs')
      broadcast cs' (encodeCMs $ clientMsgs g' V.empty)
      handle conn state pId
    _ -> do
      sendText conn ":signin"
      singIn gmRef conn

handle :: WS.Connection -> MVar ServerState -> Int -> IO ()
handle conn state pId = forever $ do
  msg      <- WS.receiveData conn
  (gs, cs) <- readMVar state
  let action    = parseMessage msg
      (gs', cm) =
        case action of
          Join n _  -> error $ "Cannot reset player name to: " ++ T.unpack n
          New n _   -> error "Cannot start a new game"
          Deal      -> deal gs
          Raise b   -> raise gs pId b
          Challenge -> challenge gs pId
          Count     -> count gs pId
          Say m     -> error "Not implemented yet"
          Invalid m -> error (T.unpack m)
  swapMVar state (gs', cs)
  broadcast cs (encodeCMs cm)

deal :: GameState -> (GameState, [ClientMsg])
deal gs@(GameState g _ r)
  | legal g Deal = (GameState g' hs r'', cm)
  | otherwise    = (gs, cm')
    where
      (cards, r') = runRand (replicateM (numOfPlayers g * cardsPerHand)
                  $ getRandomR (0, 9)) r
      (f, r'')    = runRand (getRandomR (0, numOfPlayers g - 1)) r'
      g'          = resetGame f g
      hs          = V.fromList $ toHand <$> chunksOf cardsPerHand cards
      cm          = clientMsgs g' hs
                  & traverse . cmError .~ ""
                  & singular (ix (g' ^. turn)) . cmButtons . bfRaise .~ True
      cm'         = clientMsgs g hs
                  & traverse . cmError .~ "Cannot deal a game in progress"

updateClientMsgs :: [ClientMsg] -> Game -> Text -> [ClientMsg]
updateClientMsgs cs g err  =
  cs & traverse . cmError .~ err
     & singular (ix (g ^. turn))
     . cmButtons
     . bfRaise .~ not ((Just $ g ^. turn) == g ^. bidder && g ^. rebid)
     & singular (ix (g ^. turn))
     . cmButtons
     . bfChallenge .~ ((Just $ g ^. turn) /= g ^. bidder)
     & singular (ix (g ^. turn))
     . cmButtons
     . bfCount .~ ((Just $ g ^. turn) == g ^. bidder)

raise :: GameState -> Int -> Bid -> (GameState, [ClientMsg])
raise gs@(GameState g hs r) pId b
  | legal g (Raise b) && g ^. turn == pId = (gs', cm)
  | otherwise = (gs, cm')
    where
      gs' = gs & stGame  .~ mkBid g b
      g'  = gs' ^. stGame
      cm  = updateClientMsgs (clientMsgs g' hs) g' ""
      cm' = updateClientMsgs (clientMsgs g hs) g e
      e   = "Either your bid is too low or you are trying to raise a re-bid."

challenge :: GameState -> Int -> (GameState, [ClientMsg])
challenge gs@(GameState g hs r) pId
  | legal g Challenge && g ^. turn == pId = (gs & stGame .~ g', cm)
  | otherwise = (gs, updateClientMsgs (clientMsgs g hs) g "Illegal Challenge")
      where
        g' = nextPlayer g
        cm = updateClientMsgs (clientMsgs g' hs) g' ""

count :: GameState -> Int -> (GameState, [ClientMsg])
count gs@(GameState g hs r) pId
  | legal g Count && g ^. turn == pId = deal (gs & stGame .~ g')
  | otherwise = (gs, updateClientMsgs (clientMsgs g hs) g "Illegal Count")
      where
        cnt    = countCard hs (g ^. bid . bidCard)
        result = g ^. bid . bidQuant <= cnt || cnt == 0
        g'     = scoreGame (g & won .~ Just result & inProgress .~ False) hs
