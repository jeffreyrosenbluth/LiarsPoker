module Main exposing (..)

import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Html.App as Html


main : Program Never
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
