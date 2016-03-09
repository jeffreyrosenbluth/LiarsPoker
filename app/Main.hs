module Main where

import           WebAPI
import           Network.Wai.Handler.Warp

main :: IO ()
main = run 8081 app
