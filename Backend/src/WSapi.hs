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
import           Control.Exception              (finally)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Random
import           Data.Monoid                    ((<>))
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as LB
import           Data.FileEmbed                 (embedDir)
import           Data.IntMap                    (IntMap, (!))
import qualified Data.IntMap                    as IM
import           Data.List.Split                (chunksOf)
import           Data.Maybe
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Read                 (decimal)
import           Data.Vector                    (Vector)
import qualified Data.Vector                    as V
import qualified Network.Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.WebSockets             as WS

type Clients   = IntMap WS.Connection
type GameState = Game (Vector Hand)
type Games     = MVar (IntMap (MVar (GameState, Clients)))
type Message   = Either Text (Game (Int, Text))

-- | Set the messages after a legal action and return it along with the
--   GameState.
actionMsgs :: GameState -> (GameState, IntMap Message)
actionMsgs gs = (gs, clientMsgs gs)

-- | Set the messages after an illegal action and return it along with the
--   GameState and an error message.
errorMsgs :: GameState -> Text -> (GameState, IntMap Message)
errorMsgs gs t = (gs, IM.fromList
               $ zip [0..] (replicate (gs ^. numPlyrs) (Left t)))

-- | A utility function to set the clienMgss to for broadcasting to each client.
clientMsgs :: GameState -> IntMap Message
clientMsgs g = IM.fromList $ map cm [0..(numOfPlayers g - 1)]
  where
    cm p =
      let h = T.pack . displayHand $ fromMaybe mempty ((g ^. variant) V.!? p)
      in  (p, Right $ (setButtonFlags g) & multiple .~ bonus g
                                         & variant .~ (p, h))

-- | The flags of the player whose turn it is are set. All of the other player's
--   flags are unchanged. These flags can be used by the front end to
--   enable / disable UI elements that allow only certain moves.
setButtonFlags :: Game a -> Game a
setButtonFlags g  = g & players .~ p
  where
    rf = not $ (Just $ g ^. turn) == g ^. bidder && g ^. rebid
    cf = (Just $ g ^. turn) /= g ^. bidder && isJust (g ^. bidder)
    nf = (Just $ g ^. turn) == g ^. bidder && g ^. won == Nothing
    df =  not (g ^. inProgress)
    i  = singular (ix (g ^. turn)) . flags
    p  = (g ^. players)
       & i . raiseFlag .~ rf
       & i . chalFlag  .~ cf
       & i . countFlag .~ nf
       & singular (ix 0) . flags . dealFlag .~ df

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

-- | Version of sendTextData specialized to Text input.
sendText :: WS.Connection -> Text -> IO ()
sendText = WS.sendTextData

-- | Send a list a messages to a list of clients.
broadcast :: Clients -> IntMap Text -> IO ()
broadcast cs ts = sequence_ $ fmap (uncurry sendText) ps
  where
    ps = IM.intersectionWith (,) cs ts

-- | Serialize a map of client messages to a map of JSON text.
encodeCMs :: IntMap Message -> IntMap Text
encodeCMs = fmap (T.pack . LB.unpack . encode)

staticApp :: Network.Wai.Application
staticApp = Static.staticApp
          $ Static.embeddedSettings $(embedDir "../Frontend/dist")

application :: Games -> WS.ServerApp
application gm pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  singIn gm conn

singIn :: Games -> WS.Connection -> IO ()
singIn gmRef conn = do
  sendText conn ":signin"
  msg <- WS.receiveData conn
  case parseMessage msg of
    New nm nPlyrs -> new gmRef conn nm nPlyrs
    Join nm gId   -> joinGame gmRef conn nm gId
    _ -> do
      sendText conn ":signin"
      singIn gmRef conn

-- | Remove a players connection and set him to a bot.
-- XXX Need to handl the case when the next player is also a bot.
-- XXX And what to do if the player is the dealer.
disconnect :: Games -> Int -> Int -> IO ()
disconnect gsRef gId pId = do
  gs <- readMVar gsRef
  (g, cs) <- takeMVar (gs ! gId)
      -- Remove the websocket connection
  let cs' = IM.delete pId cs
      -- Set player to a bot.
      p  = g ^. players & singular (ix pId) . bot .~ Just dumbBot
      -- If it's the player who is disconnecting had the turn, make a move.
      g' | g ^. turn == pId = dumbBot $ g & players .~ p
         | otherwise = g & players .~ p
  putMVar (gs ! gId) (g', cs')
  broadcast cs' (encodeCMs $ clientMsgs g')

new :: Games -> WS.Connection -> Text -> Int -> IO ()
new gmRef conn nm nPlyrs = do
  gm <- takeMVar gmRef
  let key = if null gm then 0 else 1 + (maximum . IM.keys $ gm)
      g = addPlayer (newGame key nPlyrs) nm
      gmState = g
      im = IM.singleton 0 conn
  gs <- newMVar (gmState, im)
  putMVar gmRef (IM.insert key gs gm)
  broadcast im (encodeCMs $ clientMsgs g)
  finally (handle conn gs 0) (disconnect gmRef key 0)

joinGame :: Games -> WS.Connection -> Text -> Int -> IO ()
joinGame gmRef conn nm gId = do
  gm <- readMVar gmRef
  -- Only try to joinGame if the 'gameId' is in the 'Games'
  when (IM.member gId gm) $ do
    let gmState = gm ! gId
    (g, cs) <- takeMVar gmState
    let pId = numOfPlayers g
        g'  = addPlayer g nm
        cs' = IM.insert pId conn cs
    if | V.length (g' ^. players) == g' ^. numPlyrs -> do
           (gs, cm) <- actionMsgs <$> evalRandIO (deal g')
           putMVar gmState (gs, cs')
           broadcast cs' (encodeCMs cm)
           finally (handle conn gmState pId) (disconnect gmRef gId pId)
       | V.length (g' ^. players) < g' ^. numPlyrs -> do
           putMVar gmState (g', cs')
           broadcast cs' (encodeCMs $ clientMsgs g')
           finally (handle conn gmState pId) (disconnect gmRef gId pId)
       | otherwise -> putMVar gmState (g, cs)

handle :: WS.Connection -> MVar (GameState, Clients) -> Int -> IO ()
handle conn gmState pId = forever $ do
  msg <- WS.receiveData conn
  (gs, cs) <- readMVar gmState
  r <- newStdGen
  let action    = parseMessage msg
      yes       = legal gs action pId
      exec = case action of
        Join n _        -> Left $ "Cannot reset player name to: " <> n
        New _ _         -> Left "Cannot start a new game"
        Deal | yes      -> Right $ evalRand (deal gs) r
        Raise b | yes   -> Right $ mkBid gs b
        Challenge | yes -> Right $ nextPlayer gs
        Count | yes     -> Right $ count gs
        Say _           -> Left "Not implemented yet"
        Invalid m       -> Left $ "Cannot parse message: " <> m
        _               -> Left $ "Illegal action: " <> T.pack (show action)

      {- If the player with the turn is a bot, then let the bot make a move.
         We assume that the bot can only make legal moves.
         There must be at least one human player, otherwise the server will
         loop forever. -}
      (gs', cm) = case exec of
        Left t  -> errorMsgs gs t
        Right s -> actionMsgs s
      (gs'', cm') = go (gs', cm)
      go (g, c) = maybe (g,c)
                        (\move -> go (let g' = move g
                                      in (g', clientMsgs g')))
                        (g ^. players . singular (ix (g ^. turn)) . bot)
  swapMVar gmState (gs'', cs)
  broadcast cs (encodeCMs cm')

deal :: (RandomGen g) => GameState -> Rand g GameState
deal g = do
  cards <- replicateM (numOfPlayers g * cardsPerHand) $ getRandomR (0, 9)
  f <- getRandomR (0, numOfPlayers g - 1)
  let g' = resetGame f g
      hs = V.fromList $ toHand <$> chunksOf cardsPerHand cards
  return $ g' & variant .~ hs

count :: GameState -> GameState
count g = g'
  where
    cnt    = countRank (g ^. variant) card
    result = g ^. bid . bidQuant <= cnt || cnt == 0
    g'     = scoreGame (g & won .~ Just result & inProgress .~ False)
    b      = g ^. bid
    card   = b ^. bidRank

dumbBot :: GameState -> GameState
dumbBot g
  | (Just $ g ^. turn) == g ^. bidder = nextPlayer . count $ g
  | otherwise = nextPlayer g
