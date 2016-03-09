{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WebAPI where

import LiarsPoker

import Control.Lens
import           Network.Wai
import           Servant
import           Servant.API

type GameAPI = "game" :> "new-game" :> Post '[JSON] Game

type UserAPI = "user" :> Capture "gameId" Integer
                      :> Capture "playerId" Int
                      :> Get '[JSON] Player

type API = GameAPI :<|> UserAPI

server :: Server API
server = game :<|> player
  where
    game = return game5
    player gId pId = return $ game5 ^. players . singular (ix 0)

api :: Proxy API
api = Proxy

app :: Application
app = serve api server
