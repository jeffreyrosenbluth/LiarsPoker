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

clientMsgs :: Game -> PrevGame -> Hands -> Text -> [ClientMsg]
clientMsgs g prv hs err = setButtonFlags $ map cm [0..(numOfPlayers g - 1)]
  where
    cm p =  ClientMsg
      { _cmGame = g
      , _cmMultiple = bonus g
      , _cmHand = T.pack . displayHand $ getHand hs p
      , _cmError = err
      , _cmButtons = BtnFlags False False False
      , _cmName = getPlayerName g p
      , _cmPrevGame = prv
      , _cmPlyrId = p
      }
    setButtonFlags :: [ClientMsg] -> [ClientMsg]
    setButtonFlags cs  =
      cs & singular (ix (g ^. turn))
         . cmButtons
         . bfRaise .~ not ((Just $ g ^. turn) == g ^. bidder && g ^. rebid)
         & singular (ix (g ^. turn))
         . cmButtons
         . bfChallenge .~ ((Just $ g ^. turn) /= g ^. bidder && isJust (g ^. bidder))
         & singular (ix (g ^. turn))
         . cmButtons
         . bfCount .~ ((Just $ g ^. turn) == g ^. bidder)

mkPrevGame :: Game -> Hands -> PrevGame
mkPrevGame g hs = PrevGame bdr b cnt (V.fromList $ map me [0..(numOfPlayers g - 1)])
  where
    bdr  = getBidderName g
    b    = g ^. bid
    card = b ^. bidCard
    cnt  = countCard hs card
    me p = getCount card (hs V.! p)

parseTextInt :: Text -> Maybe (Text, Int)
parseTextInt t = case ns of
    []    -> Nothing
    (i:_) -> Just (nm, fst i)
  where
    (nm, gId) = T.breakOn ":-:" t
    gId' = T.drop 3 gId
    ns = reads $ T.unpack gId'

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
    New nm nPlyrs -> new gmRef conn nm nPlyrs
    Join nm gId -> joinGame gmRef conn nm gId
    _ -> do
      sendText conn ":signin"
      singIn gmRef conn

new :: MVar GameMap -> WS.Connection -> Text -> Int -> IO ()
new gmRef conn nm nPlyrs = do
  r <- getStdGen
  gm <- takeMVar gmRef
  let key   = if null gm then 0 else 1 + (maximum . keys $ gm)
      g     = addPlayer (newGame key nPlyrs) 0 nm
      gmState = GameState g V.empty r
      prv  = PrevGame "" (Bid 0 0) 0 V.empty
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
        g'  = addPlayer g pId nm
        cs' = cs ++ [conn]
    if | V.length (g ^. players) == g ^. numPlyrs - 1 -> do
           let gs = deal (GameState g' V.empty r)
               cm = dealClientMsgs gs prv
           putMVar gmState (gs, prv, cs')
           broadcast cs' (encodeCMs cm)
           handle conn gmState pId
       | V.length (g ^. players) < g ^. numPlyrs -> do
           putMVar gmState (GameState g' V.empty r, prv, cs')
           broadcast cs' (encodeCMs $ clientMsgs g' prv V.empty "")
           handle conn gmState pId
       | otherwise -> putMVar gmState (GameState g hs r, prv, cs)

dealClientMsgs :: GameState -> PrevGame -> [ClientMsg]
dealClientMsgs gs prv =
  clientMsgs (gs ^. stGame) prv (gs ^. stHands) ""
     & singular (ix (gs ^. stGame . turn))
     . cmButtons
     . bfRaise
     .~ True

handle :: WS.Connection -> MVar ServerState -> Int -> IO ()
handle conn gmState pId = forever $ do
  msg           <- WS.receiveData conn
  (gs, prv, cs) <- readMVar gmState
  let action    = parseMessage msg
      (gs0, cm) =
        if legal (gs ^. stGame) action pId then
          case action of
            Join n _  -> error $ "Cannot reset player name to: " ++ T.unpack n
            New _ _   -> error "Cannot start a new game"
            Deal      -> let gs2 = deal gs
                         in (gs2, dealClientMsgs gs2 prv)
            Raise b   -> let gs2 = gs & stGame .~ mkBid (gs ^. stGame) b
                         in (gs2, dealClientMsgs gs2 prv)
            Challenge -> let gs2 = gs & stGame .~ nextPlayer (gs ^.stGame)
                         in (gs2, dealClientMsgs gs2 prv)
            Count     -> let (gs2, prv') = count gs
                         in  (gs2, dealClientMsgs gs2 prv')
            Say _     -> error "Not implemented yet"
            Invalid m -> error (T.unpack m)
        else (gs, clientMsgs (gs ^. stGame) prv (gs ^. stHands) (T.pack $ show action))
  swapMVar gmState (gs0, prv, cs)
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
        cnt    = countCard hs (g ^. bid . bidCard)
        result = g ^. bid . bidQuant <= cnt || cnt == 0
        g'     = scoreGame (g & won .~ Just result & inProgress .~ False) hs
        prv    = mkPrevGame g hs
