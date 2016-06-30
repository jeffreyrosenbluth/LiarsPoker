module LiarsPoker.Views.SignIn exposing (..)

import LiarsPoker.Model exposing (Model, Msg(..), ServerMsg(..), showServerMsg, ClientMsg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


signInView : Model -> Html Msg
signInView m =
    div
        [ class "flex flex-column m2 border border-box"
        , style [ ( "max-width", "30em" ) ]
        ]
        [ div [ class "flex-justify border border-box" ] [ nickView m ]
        , div [ class "flex-justify border border-box" ] [ joinView m ]
        , div [ class "flex-justify border border-box" ] [ startView m ]
        ]


nickView : Model -> Html Msg
nickView m =
    div [ class "center p2 bg-white" ]
        [ div [ class "h2 p1" ] [ text "Enter your player name" ]
        , div [ class "h3 gray" ] [ text "Nickname" ]
        , input
            [ Html.Attributes.value m.name
            , onInput Name
            , class "ml1 p1"
            , style [ ( "min-width", "200px" ) ]
            ]
            []
        ]


joinView : Model -> Html Msg
joinView m =
    div [ class "flex flex-column bg-white" ]
        [ div [ class "h2 p1 center" ] [ text "Join an existing game" ]
        , div [ class "center" ]
            [ div [ class "h3 center  gray" ] [ text "Game Id" ]
            , input
                [ Html.Attributes.value m.gameId
                , onInput GameId
                , class "center p1"
                , style [ ( "width", "200px" ) ]
                ]
                []
            ]
        , div [ class "center" ]
            [ button
                [ class "btn btn-primary mr4  ml4 mt2 mb2 h4 bg-olive"
                , style [ ( "width", "200px" ) ]
                , onClick <| WSoutgoing ("join " ++ m.name ++ ":-:" ++ m.gameId)
                , disabled <| m.name == ""
                ]
                [ text "Join" ]
            ]
        ]


startView : Model -> Html Msg
startView m =
    div [ class "flex flex-column bg-white" ]
        [ div [ class "h2 p1 center" ] [ text "Start a new game" ]
        , div [ class "center" ]
            [ div [ class "h3 gray" ] [ text "Number of players" ]
            , div [ class "h2 p1" ] [ text <| toString m.numPlayers ]
            , div []
                [ button
                    [ class "btn btn-outline h6 m1"
                    , onClick (NumPlayers <| Basics.max 0 (m.numPlayers - 1))
                    ]
                    [ text "-" ]
                , button
                    [ class "btn btn-outline h6 m1"
                    , onClick (NumPlayers <| m.numPlayers + 1)
                    ]
                    [ text "+" ]
                ]
            , button
                [ class "btn btn-primary mr4  ml4 mt2 mb2 h4 bg-olive"
                , style [ ( "width", "200px" ) ]
                , onClick <| WSoutgoing ("new " ++ m.name ++ ":-:" ++ toString m.numPlayers)
                , disabled <| m.name == "" || m.numPlayers == 0
                ]
                [ text "Start" ]
            ]
        ]
