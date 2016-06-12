module Main where

import           LiarsPoker
import           WSapi

import           Control.Concurrent.MVar
import           Data.Vector                    (empty)
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets             as WS
import           System.Random                  (getStdGen)

main :: IO ()
main = do
  g <- getStdGen
  state <- newMVar (GameState newGame empty g, mempty)
  Warp.runSettings
    (Warp.setPort 9160 Warp.defaultSettings)
    $ WaiWS.websocketsOr WS.defaultConnectionOptions (application state) staticApp
