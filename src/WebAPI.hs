{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WebAPI where

import LiarsPoker

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad.IO.Class
import           Network.Wai
import           Servant
import           Servant.API
import           System.IO.Unsafe (unsafePerformIO)
import           System.Random


type GameNewAPI = "game"
               :> "new-game"
               :> ReqBody '[JSON] [String]
               :> Post '[JSON] Integer

type GameShowAPI = "game"
                :> "display"
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

type API = GameNewAPI
      :<|> GameShowAPI
      :<|> PlayerAPI
      :<|> CurrentBidAPI
      :<|> ActionAPI

server :: MVar Game -> Server API
server gRef = gameNew gRef :<|> gameShow gRef
                           :<|> player gRef
                           :<|> currentBid gRef
                           :<|> action gRef
    where
      gameNew gr p = do
        sg <- liftIO getStdGen
        let newG = newGame sg 0 p
        liftIO $ putMVar gr newG
        return 0

      gameShow gr gId = do
        g <- liftIO $ readMVar gr
        return g

      player gr gId pId = do
        g <- liftIO $ readMVar gr
        return $ g ^. players . singular (ix pId)

      currentBid gr gId = do
        g <- liftIO $ readMVar gr
        return $ g ^. bid

      action gr gId a = do
        g <- liftIO $ readMVar gr
        if legal g a
          then do
            let g' = move g a
            liftIO $ swapMVar gr g'
            return $ g' ^. turn
          else
            return $ g ^. turn

api :: Proxy API
api = Proxy

app :: Application
app = serve api (server (unsafePerformIO newEmptyMVar))
