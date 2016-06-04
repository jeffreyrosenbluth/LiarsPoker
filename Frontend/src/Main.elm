module Main exposing (..)

import LiarsPoker exposing (..)
import Html.App as Html

main : Program Never
main = Html.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }
