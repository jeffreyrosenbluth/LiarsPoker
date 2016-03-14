{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import LiarsPoker
import WebAPI

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Language.Javascript.JQuery
import           Servant.API
import           Servant.JQuery

apiJS :: String
apiJS = jsForAPI  api

main :: IO ()
main = writeFile "api.js" apiJS
