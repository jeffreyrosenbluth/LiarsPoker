module Main where

import           Control.Concurrent.MVar
import           WebAPI
import           Network.Wai.Handler.Warp

main :: IO ()
main = do
  gs <- newEmptyMVar
  run 8081 $ app gs
