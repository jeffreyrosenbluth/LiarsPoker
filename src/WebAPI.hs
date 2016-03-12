{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WebAPI where

import LiarsPoker

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad (replicateM)
import           Control.Monad.Random
import           Network.Wai
import           Servant
import           Servant.API
import           System.Random

type NewAPI = "new"
            :> ReqBody '[JSON] Integer
            :> Post '[JSON] Integer

type JoinAPI = "join"
             :> ReqBody '[JSON] String
             :> Post '[JSON] Int

type DisplayAPI  = "display"
                :> Capture "gameId" Integer
                :> Get '[JSON] Game

type PlayerAPI = "player"
               :> Capture "gameId" Integer
               :> Capture "playerIdx" Int
               :> Get '[JSON] Player

type CurrentBidAPI = "current-bid"
                  :> Capture "gameId" Integer
                  :> Get '[JSON] Bid

type ActionAPI = "action"
               :> Capture "gameId" Integer
               :> ReqBody '[JSON] Action
               :> Post '[JSON] Int

type API = NewAPI
      :<|> JoinAPI
      :<|> DisplayAPI
      :<|> PlayerAPI
      :<|> CurrentBidAPI
      :<|> ActionAPI

server :: MVar (Game, StdGen) -> Server API
server gRef = new gRef :<|> join gRef
                       :<|> display gRef
                       :<|> player gRef
                       :<|> currentBid gRef
                       :<|> action gRef
    where
      new gr gId = do
        sg <- liftIO getStdGen
        liftIO $ putMVar gr (newGame gId, sg)
        return gId

      join gr plyr = do
        (g, r) <- liftIO $ readMVar gr
        let (h, r') = runRand (replicateM cardsPerHand $ getRandomR (0, 9)) r
        let newG = addPlayer g plyr (toHand h)
        liftIO $ swapMVar gr (newG, r')
        return $ newG ^. numOfPlayers

      display gr gId = do
        (g, _) <- liftIO $ readMVar gr
        return g

      player gr gId pId = do
        (g, _) <- liftIO $ readMVar gr
        return $ g ^. players . singular (ix pId)

      currentBid gr gId = do
        (g, _) <- liftIO $ readMVar gr
        return $ g ^. bid

      action gr gId a = do
        (g, r) <- liftIO $ readMVar gr
        if legal g a
          then do
            let g' = move g a
            liftIO $ swapMVar gr (g', r)
            return $ g' ^. turn
          else
            return $ g ^. turn

api :: Proxy API
api = Proxy

app :: MVar (Game, StdGen) -> Application
app gs = serve api (server gs)
