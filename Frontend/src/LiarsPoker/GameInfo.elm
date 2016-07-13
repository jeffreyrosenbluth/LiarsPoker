module LiarsPoker.GameInfo exposing (..)

import LiarsPoker.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { name : String
    , turn : String
    , bidder : String
    , baseStake : Int
    , multiple : Int
    , bid : Bid
    , hand : String
    }

view : Model -> Html ()
view model =
  div []
      [ div [ class "flex-justify border border-box" ]
            [ currentPlayerView model
            , handView model
            ]
      , div [ class "flex bg-white" ]
          [ div [ style [ ( "width", "50%" ) ] ] [ bidderView model ]
          , div [ style [ ( "width", "50%" ) ] ] [ playerView model ]
          ]
      , div [ class "flex bg-white" ]
          [ div [ style [ ( "width", "50%" ) ] ] [ stakesView model ]
          , div [ style [ ( "width", "50%" ) ] ] [ multipleView model ]
          ]
      , div [ class "bg-white" ] [ bidView model ]
    ]

currentPlayerView : Model -> Html ()
currentPlayerView model =
    let
        icon =
            if model.name == model.turn then
                i [ class "fa fa-circle-o-notch fa-spin ml1 olive" ] []
            else
                i [ class "fa fa-circle-o-notch ml1 olive muted" ] []
    in
        div
            [ class "center p1 h1 bold border border-box"
            , style [ ( "background-color", "white" ) ]
            ]
            [ text model.name
            , icon
            ]


handView : Model -> Html ()
handView model =
    div
        [ class "center p1 h1"
        , style
            [ ( "color", "darkgreen" )
            , ( "background-image", "url(\"DollarBill.png\")" )
            , ( "max-height", "100%" )
            ]
        ]
        [ div [ class "mt4" ] [ text <| "L\x2004" ++ model.hand ++ "\x2004P" ] ]


bidderView : Model -> Html ()
bidderView model =
    div [ class "flex bg-white" ]
        [ div [ class "ml1 p1 h2 gray" ] [ text "Bidder" ]
        , div [ class "p1 h2" ] [ text model.bidder ]
        ]


bidView : Model -> Html ()
bidView model =
    div [ class "flex bold", style [ ( "background-color", "#D0C6AD" ) ] ]
        [ div [ class "flex-auto" ] []
        , div [ class "p1 h2", style [ ( "color", "navy" ) ] ] [ text "Bid" ]
        , div [ class "p1 h2", style [ ( "color", "navy" ) ] ]
            [ text
                <| toString model.bid.bidQuant
                ++ " "
                ++ toString model.bid.bidCard
                ++ "s"
            ]
        , div [ class "flex-auto" ] []
        ]


stakesView : Model -> Html ()
stakesView model =
    div [ class "flex bg-white" ]
        [ div [ class "ml1 p1 h2 gray" ] [ text "Base Stake" ]
        , div [ class "p1 h2" ] [ text <| toString model.baseStake ]
        ]


multipleView : Model -> Html ()
multipleView model =
    div [ class "flex bg-white" ]
        [ div [ class "p1 h2 gray" ] [ text "Multiple" ]
        , div [ class "p1 h2" ] [ text <| toString model.multiple ]
        ]


playerView : Model -> Html ()
playerView model =
    div [ class "flex bg-white" ]
        [ div [ class "p1 h2 gray" ] [ text "Player" ]
        , div [ class "p1 h2" ] [ text <| model.turn ]
        ]
