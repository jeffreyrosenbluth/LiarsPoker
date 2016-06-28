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
        [ div [ class "h2 ml1 p1 gray" ] [ text "Nickname" ]
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
        [ div [ class "p1 center red" ] [ text "Join an existing game" ]
        , div [ class "center" ]
            [ div [ class "h2 center ml2 p1 gray" ] [ text "Game Id" ]
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
                [ class "btn btn-primary mr4  ml4 mt2 mb2 h6"
                , style [ ( "width", "200px" ) ]
                , onClick <| WSoutgoing ("name " ++ m.name)
                ]
                [ text "Join" ]
            ]
        ]


startView : Model -> Html Msg
startView m =
    div [ class "flex flex-column bg-white" ]
        [ div [ class "p1 center red" ] [ text "Start a new game" ]
        , div [ class "center" ]
            [ div [ class "h2 ml2 p1 gray" ] [ text "Number of players" ]
            , input
                [ placeholder "3"
                  -- , Html.Attributes.value m.test
                  -- , onInput Test
                , class "center p1"
                , style [ ( "width", "200px" ) ]
                ]
                []
            ]
        , div [ class "center" ]
            [ button
                [ class "btn btn-primary mr4  ml4 mt2 mb2 h6"
                , style [ ( "width", "200px" ) ]
                  -- , onClick <| WSoutgoing ("name " ++ m.test)
                ]
                [ text "Start" ]
            ]
        ]
