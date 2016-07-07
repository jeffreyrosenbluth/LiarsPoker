module Main where

import           WSapi

import           Control.Concurrent.MVar
import           Data.IntMap                    (empty)
import           Network.Wai.Handler.Warp       (defaultSettings, runSettings,
                                                 setPort)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.WebSockets             (defaultConnectionOptions)
import           System.Environment

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 9160 read $ lookup "PORT" env
  gmRef <- newMVar empty
  runSettings ( setPort port defaultSettings )
              ( websocketsOr  defaultConnectionOptions
                            ( application gmRef )
                              staticApp
              )
