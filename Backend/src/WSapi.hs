{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

----------------------------------------------------------
-- |
-- Websockets API for LiarsPoker multiplayer game
-- (c) 2016 Jeffrey Rosenbluth
--------------------------------------------------------

module WSapi where

import           LiarsPoker
import           Types

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad
import           Control.Monad.Random
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as LB
import           Data.FileEmbed                 (embedDir)
import           Data.IntMap                    (IntMap, insert, keys, member, (!))
import           Data.List.Split                (chunksOf)
import           Data.Maybe
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Read                 (decimal)
import qualified Data.Vector                    as V
import qualified Network.Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.WebSockets             as WS


type ServerState = (GameState, PrevGame, Clients)
type GameMap = IntMap (MVar ServerState)

-- | Set the clientMsgs after and legal action and return it along with the
--   GameState.
actionMsgs :: GameState -> PrevGame -> (GameState, [ClientMsg])
actionMsgs gs prv = (gs, cm)
  where
    cm = clientMsgs (gs ^. stGame) prv (gs ^. stHands) ""

-- | A utility function to set the clienMgss to for broadcasting to each client.
clientMsgs :: Game -> PrevGame -> Hands -> Text -> [ClientMsg]
clientMsgs g prv hs err = setButtonFlags g $ map cm [0..(numOfPlayers g - 1)]
  where
    cm p =
      let Just nm = getPlayerName g p
      in  ClientMsg
            { _cmGame = g
            , _cmMultiple = bonus g
            , _cmHand = T.pack . displayHand $ getHand hs p
            , _cmError = err
            , _cmButtons = BtnFlags False False False
            , _cmName = nm
            , _cmPrevGame = prv
            , _cmPlyrId = p
            }

setButtonFlags :: Game -> [ClientMsg] -> [ClientMsg]
setButtonFlags g cs  =
  cs & singular (ix (g ^. turn))
     . cmButtons
     . bfRaise .~ not ((Just $ g ^. turn) == g ^. bidder && g ^. rebid)
     & singular (ix (g ^. turn))
     . cmButtons
     . bfChallenge .~ ((Just $ g ^. turn) /= g ^. bidder && isJust (g ^. bidder))
     & singular (ix (g ^. turn))
     . cmButtons
     . bfCount .~ ((Just $ g ^. turn) == g ^. bidder)

-- | Parse a cleint message of the form "cmd name:-:n", e.g. "join Jeff:-:3".
parseTextInt :: Text -> Maybe (Text, Int)
parseTextInt t = case ns of
    []    -> Nothing
    (i:_) -> Just (nm, fst i)
  where
    (nm, gId) = T.breakOn ":-:" t
    gId' = T.drop 3 gId
    ns = reads $ T.unpack gId'

-- | Convert plain text websocket messages to 'Action's.
parseMessage :: Text -> Action
parseMessage t
  | "join " `T.isPrefixOf` t =
      case parseTextInt (T.drop 5 t) of
        Nothing -> Invalid "Client send an invalid joinGame message"
        Just (n, i) -> Join n i
  | "new " `T.isPrefixOf` t =
      case parseTextInt (T.drop 4 t) of
        Nothing -> Invalid "Client send an invalid new message"
        Just (n, i) -> New n i
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
staticApp = Static.staticApp
          $ Static.embeddedSettings $(embedDir "../Frontend/dist")

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
    New nm nPlyrs -> new gmRef conn nm nPlyrs
    Join nm gId   -> joinGame gmRef conn nm gId
    _ -> do
      sendText conn ":signin"
      singIn gmRef conn

new :: MVar GameMap -> WS.Connection -> Text -> Int -> IO ()
new gmRef conn nm nPlyrs = do
  r  <- getStdGen
  gm <- takeMVar gmRef
  let key     = if null gm then 0 else 1 + (maximum . keys $ gm)
      g       = addPlayer (newGame key nPlyrs) nm
      gmState = GameState g V.empty r
      prv     = PrevGame "" (Bid 0 0) 0 V.empty
  gs <- newMVar (gmState, prv, [conn])
  putMVar gmRef (insert key gs gm)
  broadcast [conn] (encodeCMs $ clientMsgs g prv V.empty "")
  handle conn gs 0

joinGame :: MVar GameMap -> WS.Connection -> Text -> Int -> IO ()
joinGame gmRef conn nm gId = do
  gm <- readMVar gmRef
  -- Only try to joinGame if the 'gameId' is in the 'GameMap'
  when (member gId gm) $ do
    let gmState = gm ! gId
    (GameState g hs r, prv, cs) <- takeMVar gmState
    let pId = numOfPlayers g
        g'  = addPlayer g nm
        cs' = cs ++ [conn]
    if | V.length (g ^. players) == g ^. numPlyrs - 1 -> do
           let (gs, cm) = actionMsgs (deal (GameState g' V.empty r)) prv
           putMVar gmState (gs, prv, cs')
           broadcast cs' (encodeCMs cm)
           handle conn gmState pId
       | V.length (g ^. players) < g ^. numPlyrs -> do
           putMVar gmState (GameState g' V.empty r, prv, cs')
           broadcast cs' (encodeCMs $ clientMsgs g' prv V.empty "")
           handle conn gmState pId
       | otherwise -> putMVar gmState (GameState g hs r, prv, cs)

handle :: WS.Connection -> MVar ServerState -> Int -> IO ()
handle conn gmState pId = forever $ do
  msg           <- WS.receiveData conn
  (gs, prv, cs) <- readMVar gmState
  let action    = parseMessage msg
      (newGS, cm) =
        if legal (gs ^. stGame) action pId then
          case action of
            Join n _  -> error $ "Cannot reset player name to: " ++ T.unpack n
            New _ _   -> error "Cannot start a new game"
            Deal      -> actionMsgs (deal gs) prv
            Raise b   -> actionMsgs (gs & stGame .~ mkBid (gs ^. stGame) b) prv
            Challenge -> actionMsgs (gs & stGame .~ nextPlayer (gs ^.stGame)) prv
            Count     -> uncurry actionMsgs $ count gs
            Say _     -> error "Not implemented yet"
            Invalid m -> error (T.unpack m)
        else (gs, clientMsgs (gs ^. stGame) prv (gs ^. stHands) (T.pack $ show action))
  swapMVar gmState (newGS, prv, cs)
  broadcast cs (encodeCMs cm)

deal :: GameState -> GameState
deal (GameState g _ r) = GameState g' hs r''
    where
      (cards, r') = runRand (replicateM (numOfPlayers g * cardsPerHand)
                  $ getRandomR (0, 9)) r
      (f, r'')    = runRand (getRandomR (0, numOfPlayers g - 1)) r'
      g'          = resetGame f g
      hs          = V.fromList $ toHand <$> chunksOf cardsPerHand cards

count :: GameState -> (GameState, PrevGame)
count gs@(GameState g hs _) = (deal (gs & stGame .~ g'), prv)
  where
    cnt    = countRank hs card
    result = g ^. bid . bidQuant <= cnt || cnt == 0
    g'     = scoreGame (g & won .~ Just result & inProgress .~ False) hs
    prv    = PrevGame bdr b cnt (V.fromList $ map me [0..(numOfPlayers g - 1)])
    bdr    = getBidderName g
    b      = g ^. bid
    card   = b ^. bidRank
    me p   = getCount card (hs V.! p)
