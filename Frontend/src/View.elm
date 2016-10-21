module View exposing (..)

{-| The top level view
-}

import Model exposing (Model, Msg(..), ServerMsg(..), showServerMsg, ClientMsg, higher)
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
            mainView model cm

        ErrorMsg e ->
            div [ class "h2 p2 m2 red" ] [ text "Cannot parse server message" ]


mainView : Model -> ClientMsg -> Html Msg
mainView m c =
    div
        [ class "flex flex-column m2 border border-box"
        , style [ ( "max-width", "40rem" ) ]
        ]
        [ GameInfo.view m.gameInfo
        , PlayerList.view m.players
        , App.map GamePlay (GamePlay.view m.gamePlay)
        , div [ class "p1 center red" ] [ text <| showServerMsg m.wsIncoming ]
        , previousHandView m c
        , if c.cmHand == "" then
            waitingView c
          else
            div [] []
        , rulesView
        ]


waitingView : ClientMsg -> Html Msg
waitingView c =
    div [ class "flex p2 m2 border" ]
        [ div [ class "p1 h3 red" ]
            [ text <| "Waiting for players"
            , i [ class "fa fa-spinner fa-spin fa-lg ml1 red" ] []
            ]
        , div [ class "flex-auto" ] []
        , div [ class "p1 h3" ] [ text <| "Game Id " ++ toString c.cmGame.gameId ]
        , div [ class "flex-auto" ] []
        , div [ class "p1 h4 olive italic" ] [ text "Use this to invite players" ]
        ]


previousHandView : Model -> ClientMsg -> Html Msg
previousHandView m c =
    let
        nm =
            c.cmPrevGame.pgBidder

        bd =
            toString c.cmPrevGame.pgBid.bidQuant
                ++ " "
                ++ toString c.cmPrevGame.pgBid.bidRank
                ++ "s"

        you =
            toString <| withDefault 0 <| get c.cmPlyrId c.cmPrevGame.pgMe

        total =
            toString c.cmPrevGame.pgCount

        result =
            if c.cmPrevGame.pgCount >= c.cmPrevGame.pgBid.bidQuant then
                "won"
            else
                "lost"

        visible =
            if m.gamePlay.preResult then
                "inline-block"
            else
                "none"
    in
        div
            [ class "gray bg-white m2"
            , style
                [ ( "z-index", "1" )
                , ( "position", "fixed" )
                , ( "top", "10rem" )
                , ( "display", visible )
                , ( "height", "20rem" )
                , ( "max-width", "38em" )
                , ( "color", "darkgreen" )
                ]
            ]
            [ p [ class "m2 h3" ]
                [ text <| nm ++ " " ++ result ++ " with a bid of " ++ bd ]
            , p [ class "ml2 h3" ]
                [ text <| "You had " ++ you ]
            , p [ class "ml2 h3" ]
                [ text <| "There were " ++ total ++ " total" ]
            , button
                [ class "btn btn-primary bg-olive h6 right m1 ml4 mr4"
                , onClick <| GamePlay <| GamePlay.PreResult False
                ]
                [ text "X" ]
            ]


rulesView : Html Msg
rulesView =
    div [ class "m1 center blue" ]
        [ a [ class "underline", target "_blank", href "http://www.liars-poker.com" ]
            [ text "Rules" ]
        ]
