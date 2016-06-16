module Main exposing (..)

import LiarsPoker.Model exposing (..)
import LiarsPoker.Update exposing (..)
import LiarsPoker.View exposing (..)
import Html.App as Html

main : Program Never
main = Html.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }
