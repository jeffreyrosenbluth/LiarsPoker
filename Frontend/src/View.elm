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
        [ App.map None (GameInfo.view m.gameInfo)
        , App.map None (PlayerList.view m.players)
        , App.map GamePlay (GamePlay.view m.gamePlay)
        , div [ class "p1 center red" ] [ text <| showServerMsg m.wsIncoming ]
        , previousHandView c
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


previousHandView : ClientMsg -> Html Msg
previousHandView c =
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
    in
        div [ class "m1 center italic blue" ]
            [ text
                <| if nm == "" then
                    ""
                   else
                    nm
                        ++ " "
                        ++ result
                        ++ " with a bid of "
                        ++ bd
                        ++ ". ◆ You had "
                        ++ you
                        ++ ". ◆ There were "
                        ++ total
                        ++ " total."
            ]


rulesView : Html Msg
rulesView =
    div [ class "m1 center blue" ]
        [ a [ class "underline", target "_blank", href "http://www.liars-poker.com" ]
            [ text "Rules" ]
        ]
