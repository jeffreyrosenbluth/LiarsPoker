module GameInfo exposing (..)

import Types exposing (..)
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


init : Model
init =
    { name = ""
    , turn = ""
    , bidder = ""
    , baseStake = 1
    , multiple = 1
    , bid = Bid 0 0
    , hand = ""
    }


view : Model -> Html msg
view model =
    div []
        [ div [ class "flex-justify border border-box" ]
            [ currentPlayerView model
            , handView model
            ]
        , div [ class "flex bg-white" ]
            [ div [ style [ ( "width", "50%" ) ] ] [ stakesView model ]
            , div [ style [ ( "width", "50%" ) ] ] [ multipleView model ]
            ]
        , div [ class "bg-white" ] [ bidView model ]
        ]


currentPlayerView : Model -> Html msg
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


handView : Model -> Html msg
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


bidderView : Model -> Html msg
bidderView model =
    div [ class "flex bg-white" ]
        [ div [ class "ml1 p1 h2 gray" ] [ text "Bidder" ]
        , div [ class "p1 h2" ] [ text model.bidder ]
        ]


bidView : Model -> Html msg
bidView model =
    div [ class "flex bold", style [ ( "background-color", "#D0C6AD" ) ] ]
        [ div [ class "flex-auto" ] []
        , div [ class "p1 h2", style [ ( "color", "navy" ) ] ]
              [ text model.bidder ]
        , div [ class "p1 h2", style [ ( "color", "navy" ) ] ]
            [ text <|
                if   model.bidder == "" then ""
                else toString model.bid.bidQuant
                     ++ " "
                     ++ toString model.bid.bidRank
                     ++ "s"
            ]
        , div [ class "flex-auto" ] []
        ]


stakesView : Model -> Html msg
stakesView model =
    div [ class "flex bg-white" ]
        [ div [ class "ml1 p1 h2 gray" ] [ text "Base Stake" ]
        , div [ class "p1 h2" ] [ text <| toString model.baseStake ]
        ]


multipleView : Model -> Html msg
multipleView model =
    div [ class "flex bg-white" ]
        [ div [ class "p1 h2 gray" ] [ text "Multiple" ]
        , div [ class "p1 h2" ] [ text <| toString model.multiple ]
        ]


playerView : Model -> Html msg
playerView model =
    div [ class "flex bg-white" ]
        [ div [ class "p1 h2 gray" ] [ text "Player" ]
        , div [ class "p1 h2" ] [ text <| model.turn ]
        ]
