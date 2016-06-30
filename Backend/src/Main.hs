module Main where

import           WSapi

import           Control.Concurrent.MVar
import           Data.IntMap                    (empty)
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets             as WS
import           System.Environment

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 9160 read $ lookup "PORT" env
  gmRef <- newMVar empty
  Warp.runSettings
    (Warp.setPort port Warp.defaultSettings)
    $ WaiWS.websocketsOr WS.defaultConnectionOptions
                         (application gmRef)
                         staticApp
