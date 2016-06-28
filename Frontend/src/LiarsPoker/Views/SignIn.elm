module LiarsPoker.Views.SignIn exposing (..)

import LiarsPoker.Model exposing (Model, Msg(..), ServerMsg(..), showServerMsg, ClientMsg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

signInView : Model -> Html Msg
signInView m =
  div
    [ class "flex flex-column m2 border border-box"
    , style [ ( "max-width", "40em" ) ]
    ]
    [ div [ class "flex-justify border border-box" ] [ nickView m ]
    , div [ class "flex-justify border border-box" ] [ joinView m ]
    , div [ class "flex-justify border border-box" ] [ startView m ]
    ]

nickView : Model -> Html Msg
nickView m =
  div [ class "flex bg-white" ]
      [ div [ class "h2 ml1 p1 gray" ] [ text "Nickname" ]
      , input
          [ placeholder "John"
          , Html.Attributes.value m.test
          , onInput Test
          , class "ml1 p1"
          , style [ ( "min-width", "200px" ) ]
          ]
          []
      ]

joinView : Model -> Html Msg
joinView m =
  div [ class "flex flex-column bg-white" ]
      [ div [ class "p1 center red" ] [ text "Join an existing game" ]
      , div [ class "flex flex-row"]
        [ div [ class "h2 center p1 gray" ] [ text "Game Id" ]
        , input
            [ placeholder "ABC123"
            -- , Html.Attributes.value m.test
            -- , onInput Test
            , class "center p1"
            , style [ ( "width", "200px" ) ]
            ]
            []
         ]
      , button
          [ class "btn btn-outline mr4  ml4 mt2 mb2 h6"
          , style [ ( "width", "200px" ) ]
          , onClick <| WSoutgoing ("name " ++ m.test)
          ]
          [ text "Join" ]
      ]

startView : Model -> Html Msg
startView m =
  div [ class "flex flex-column bg-white" ]
      [ div [ class "p1 center red" ] [ text "Start a new game" ]
      , div [ class "flex flex-row"]
        [ div [ class "h2 center p1 gray" ] [ text "Number of players" ]
        , input
            [ placeholder "3"
            -- , Html.Attributes.value m.test
            -- , onInput Test
            , class "center p1"
            , style [ ( "width", "200px" ) ]
            ]
            []
         ]
      , button
          [ class "btn btn-outline mr4  ml4 mt2 mb2 h6"
          , style [ ( "width", "200px" ) ]
          -- , onClick <| WSoutgoing ("name " ++ m.test)
          ]
          [ text "Start" ]
      ]
