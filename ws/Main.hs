module Main where

import           WSapi

import           Control.Concurrent.MVar
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

main :: IO ()
main = do
  state <- newEmptyMVar
  Warp.runSettings
    (Warp.setPort 9160 Warp.defaultSettings)
    $ WaiWS.websocketsOr WS.defaultConnectionOptions (application state) staticApp
