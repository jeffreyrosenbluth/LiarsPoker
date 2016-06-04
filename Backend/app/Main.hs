module Main where

import           WSapi
import           LiarsPoker

import           Control.Concurrent.MVar
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import           System.Random (getStdGen)

main :: IO ()
main = do
  g <- getStdGen
  state <- newMVar (newGame, g, mempty)
  Warp.runSettings
    (Warp.setPort 9160 Warp.defaultSettings)
    $ WaiWS.websocketsOr WS.defaultConnectionOptions (application state) staticApp
