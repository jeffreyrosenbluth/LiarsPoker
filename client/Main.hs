{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API
import Servant.Client

main :: IO ()
main = putStrLn "I'm the client"
