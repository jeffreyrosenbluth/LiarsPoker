module View exposing (..)

{-| The top level view
-}

import Model exposing ( Model, Msg(..), ServerMsg(..), showServerMsg, Message
                      , Game, higher)
import PlayerList as PlayerList
import GameInfo as GameInfo
import GamePlay as GamePlay
import SignIn as SignIn
import Array as A exposing (Array, get)
import Maybe as M exposing (withDefault)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


view : Model -> Html Msg
view model =
    case model.wsIncoming of
        RawMsg ":signin" ->
            App.map SignIn (SignIn.view model.signIn)

        RawMsg s ->
            div [ class "h2 p2 m2 red" ] [ text <| "Illegal Raw Message: " ++ s ]

        JsonMsg cm ->
            case cm of
                Ok g -> mainView model g
                Err s ->
                    div [ class "h2 p2 m2 red" ] [ text s ]

        ErrorMsg e ->
            div [ class "h2 p2 m2 red" ] [ text "Cannot parse server message" ]


mainView : Model -> Game -> Html Msg
mainView m g =
    div
        [ class "flex flex-column m2 border border-box"
        , style [ ( "max-width", "40rem" ) ]
        ]
        [ GameInfo.view m.gameInfo
        , PlayerList.view m.players
        , App.map GamePlay (GamePlay.view m.gamePlay)
        , div [ class "p1 center red" ] [ text <| showServerMsg m.wsIncoming ]
        , if g.hands == "" then
            waitingView g
          else
            div [] []
        , rulesView
        ]


waitingView : Game -> Html Msg
waitingView g =
    div [ class "flex p2 m2 border" ]
        [ div [ class "p1 h3 red" ]
            [ text <| "Waiting for players"
            , i [ class "fa fa-spinner fa-spin fa-lg ml1 red" ] []
            ]
        , div [ class "flex-auto" ] []
        , div [ class "p1 h3" ] [ text <| "Game Id " ++ toString g.gameId ]
        , div [ class "flex-auto" ] []
        , div [ class "p1 h4 olive italic" ] [ text "Use this to invite players" ]
        ]


rulesView : Html Msg
rulesView =
    div [ class "m1 center blue" ]
        [ a [ class "underline", target "_blank", href "http://www.liars-poker.com" ]
            [ text "Rules" ]
        ]
