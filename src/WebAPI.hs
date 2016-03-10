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

type GameAPI = "game" :> "new-game"
                      :> QueryParams "players" String
                      :> Post '[JSON] Integer

type PlayerAPI = "player" :> Capture "gameId" Integer
                      :> Capture "playerIdx" Int
                      :> Get '[JSON] Player

type CurrentBidAPI = "current-bid" :> Capture "gameId" Integer
                                   :> Get '[JSON] Bid

-- type PlayAPI = "play" :>

type API = GameAPI :<|> PlayerAPI :<|> CurrentBidAPI

server :: MVar Game -> Server API
server gRef = game gRef :<|> player gRef :<|> currentBid gRef
    where
      game gr p = do
        let newG = newGame 42 0 ["Moe", "Larry", "Curly"]
        liftIO $ putMVar gr newG
        return 0

      player gr gId pId = do
        g <- liftIO $ readMVar gr
        return $ g ^. players . singular (ix pId)

      currentBid gr gId = do
        g <- liftIO $ readMVar gr
        return $ g ^. bid

api :: Proxy API
api = Proxy

app :: Application
app = serve api (server (unsafePerformIO newEmptyMVar))
