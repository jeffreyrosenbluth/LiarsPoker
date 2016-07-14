module PlayerList exposing (..)

import Types exposing (..)
import Array as A exposing (Array, get)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe as M exposing (withDefault)


type alias Model =
    { players : Array Player
    , bidder : Maybe Int
    , turn : Int
    }


init : Model
init =
    { players = A.empty, bidder = Nothing, turn = 0 }


view : Model -> Html ()
view model =
    div [ class "flex bg-white border-box border h2" ]
        [ markerListView model
        , playerListView model
        , scoreListView model
        , div [ style [ ( "width", "20%" ) ] ] []
        ]


turn : Model -> String
turn model =
    withDefault "Error" <| M.map .name <| get model.turn model.players


bidder : Model -> String
bidder model =
    let
        b =
            model.bidder `M.andThen` \n -> get n model.players
    in
        withDefault "None" (M.map .name b)


playerListView : Model -> Html ()
playerListView model =
    let
        sty x =
            if x == bidder model then
                [ style [ ( "color", "firebrick" ) ] ]
            else if x == turn model then
                [ style [ ( "color", "darkblue" ) ] ]
            else
                [ style [ ( "color", "gray" ) ] ]

        ps =
            A.toList <| A.map (\x -> li (sty x) [ text x ]) (A.map .name model.players)
    in
        ul [ class "list-reset ml2 mt1", style [ ( "width", "45%" ) ] ] ps


scoreListView : Model -> Html ()
scoreListView model =
    let
        ss =
            A.toList
                <| A.map (\x -> li [ class "right-align" ] [ text x ])
                    (A.map (toString << .score) model.players)
    in
        ul [ class "list-reset mt1", style [ ( "width", "30%" ) ] ] ss


markerListView : Model -> Html ()
markerListView model =
    let
        mark x =
            if x == turn model then
                i [ class "fa fa-play" ] []
            else
                i [ class "fa" ] []

        ps =
            A.toList <| A.map (\x -> li [] [ mark x ]) (A.map .name model.players)
    in
        ul [ class "list-reset ml2 mt1", style [ ( "width", "5%" ) ] ] ps
