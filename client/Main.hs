{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import LiarsPoker
import WebAPI

import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Servant.API
import           Servant.Client

newP        :: Integer -> EitherT ServantError IO Integer
joinP       :: String -> EitherT ServantError IO Int
dealP       :: Integer -> EitherT ServantError IO Integer
displayG    :: Integer -> EitherT ServantError IO Game
playerG     :: Integer -> Int -> EitherT ServantError IO Player
currentBidG :: Integer -> EitherT ServantError IO Bid
actionP     :: Integer -> Action -> EitherT ServantError IO Int

newP :<|> joinP
     :<|> dealP
     :<|> displayG
     :<|> playerG
     :<|> currentBidG
     :<|> actionP
        = client api (BaseUrl Http "localhost" 8081)

setup :: EitherT ServantError IO (Game)
setup = do
  newP 0
  joinP "Paul"
  joinP "John"
  joinP "George"
  joinP "Ringo"
  dealP 0
  g <- displayG 0
  return g

samplePlay1 :: EitherT ServantError IO (Game)
samplePlay1 = do
  actionP 0 (Raise (Bid C6 4))
  actionP 0 Challenge
  actionP 0 Challenge
  actionP 0 Challenge
  actionP 0 (Raise (Bid C0 4))
  g <- displayG 0
  return g

main :: IO ()
main = do
  s <- runEitherT setup
  case s of
    Left err -> putStrLn $ "Error: " ++ show err
    Right g -> do
      print g
  t <- runEitherT samplePlay1
  case t of
    Left err -> putStrLn $ "Error: " ++ show err
    Right g -> do
      print g
