module LiarsPoker.View exposing (..)

import LiarsPoker.Model exposing (Model, Msg(..), ServerMsg(..), showServerMsg, ClientMsg, higher)
import LiarsPoker.PlayerList as PlayerList
import LiarsPoker.GameInfo as GameInfo
import LiarsPoker.Views.SignIn exposing (..)
import Array as A exposing (Array, get)
import Maybe as M exposing (withDefault)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    case model.wsIncoming of
        RawMsg ":signin" ->
            signInView model

        RawMsg _ ->
            div [ class "h2 p2 m2 red" ] [ text "Illegal Raw Message." ]

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
        [ App.map GameInfo (GameInfo.view m.gameInfo)
        , App.map PlayerList (PlayerList.view m.players)
        , quantEntryView m
        , rankEntryView m
        , playView m c
        , div [ class "p1 center red" ] [ text <| showServerMsg m.wsIncoming ]
        , previousHandView c
        , if c.cmHand == "" then
            waitingView m c
          else
            div [] []
        , rulesView
        ]


quantEntryView : Model -> Html Msg
quantEntryView m =
    div [ class "flex bg-white" ]
        [ div [ class "h2 ml4 p1 gray" ] [ text "Quantity" ]
        , div [ class "flex-auto" ] []
        , div [ class "h2 p1" ] [ text <| toString m.quant ]
        , div [ class "flex-auto" ] []
        , button
            [ class "btn btn-outline m1 h6"
            , onClick (RaiseQuant <| Basics.max 0 (m.quant - 1))
            ]
            [ i [ class "fa fa-minus" ] [] ]
        , button
            [ class "btn btn-outline mt1 mb1 mr4 h6"
            , onClick (RaiseQuant <| m.quant + 1)
            ]
            [ i [ class "fa fa-plus" ] [] ]
        ]


constrainRank : Int -> Int
constrainRank n =
    if n == 10 then
        0
    else if n < 0 then
        9
    else
        n


rankEntryView : Model -> Html Msg
rankEntryView m =
    div [ class "flex bg-white" ]
        [ div [ class "h2 ml4 p1 gray" ] [ text "Rank" ]
        , div [ class "flex-auto" ] []
        , div [ class "h2 p1 ml3" ] [ text <| toString m.card ]
        , div [ class "flex-auto" ] []
        , button
            [ class "btn btn-outline m1 h6"
            , onClick <| RaiseRank <| constrainRank <| m.card - 1
            ]
            [ i [ class "fa fa-minus" ] [] ]
        , button
            [ class "btn btn-outline mt1 mb1 mr4 h6"
            , onClick <| RaiseRank <| constrainRank <| m.card + 1
            ]
            [ i [ class "fa fa-plus" ] [] ]
        ]


playView : Model -> ClientMsg -> Html Msg
playView m c =
    div [ class "flex bg-white" ]
        [ div [ class "flex-auto" ] []
        , button
            [ class "btn btn-primary m2"
            , style [ ( "background-color", "darkgreen" ) ]
            , onClick
                <| WSoutgoing
                <| "bid "
                ++ toString m.quant
                ++ " "
                ++ toString m.card
            , disabled <| not c.cmButtons.bfRaise || not (higher m c)
            ]
            [ text "Raise" ]
        , div [ class "flex-auto" ] []
        , button
            [ class "btn btn-primary m2"
            , style [ ( "background-color", "darkgreen" ) ]
            , onClick <| WSoutgoing "challenge"
            , disabled <| not c.cmButtons.bfChallenge
            ]
            [ text "Challenge" ]
        , div [ class "flex-auto" ] []
        , button
            [ class "btn btn-primary m2"
            , style [ ( "background-color", "darkgreen" ) ]
            , onClick <| WSoutgoing "count"
            , disabled <| not c.cmButtons.bfCount
            ]
            [ text "Count" ]
        , div [ class "flex-auto" ] []
        ]


waitingView : Model -> ClientMsg -> Html Msg
waitingView model c =
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
                ++ toString c.cmPrevGame.pgBid.bidCard
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
