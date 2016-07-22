module GamePlay exposing (..)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import WebSocket


type alias Model =
    { quant : Int
    , rank : Int
    , buttons : BtnFlags
    , bid : Bid
    }


type Msg
    = RaiseQuant Int
    | RaiseRank Int
    | Outgoing String


init : Model
init =
    { quant = 0
    , rank = 0
    , buttons =
        { bfRaise = False
        , bfChallenge = False
        , bfCount = False
        }
    , bid = Bid 0 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RaiseQuant q ->
            ( { model | quant = q }, Cmd.none )

        RaiseRank r ->
            ( { model | rank = r }, Cmd.none )

        Outgoing s ->
            ( model, WebSocket.send wsURL s )


view : Model -> Html Msg
view model =
    div []
        [ quantEntryView model
        , rankEntryView model
        , playView model
        ]


quantEntryView : Model -> Html Msg
quantEntryView model =
    div [ class "flex bg-white" ]
        [ div [ class "h2 ml4 p1 gray" ] [ text "Quantity" ]
        , div [ class "flex-auto" ] []
        , div [ class "h2 p1" ] [ text <| toString model.quant ]
        , div [ class "flex-auto" ] []
        , button
            [ class "btn btn-outline m1 h6"
            , onClick (RaiseQuant <| Basics.max 0 (model.quant - 1))
            ]
            [ i [ class "fa fa-minus" ] [] ]
        , button
            [ class "btn btn-outline mt1 mb1 mr4 h6"
            , onClick (RaiseQuant <| model.quant + 1)
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
rankEntryView model =
    div [ class "flex bg-white" ]
        [ div [ class "h2 ml4 p1 gray" ] [ text "Rank" ]
        , div [ class "flex-auto" ] []
        , div [ class "h2 p1 ml3" ] [ text <| toString model.rank ]
        , div [ class "flex-auto" ] []
        , button
            [ class "btn btn-outline m1 h6"
            , onClick <| RaiseRank <| constrainRank <| model.rank - 1
            ]
            [ i [ class "fa fa-minus" ] [] ]
        , button
            [ class "btn btn-outline mt1 mb1 mr4 h6"
            , onClick <| RaiseRank <| constrainRank <| model.rank + 1
            ]
            [ i [ class "fa fa-plus" ] [] ]
        ]


playView : Model -> Html Msg
playView model =
    div [ class "flex bg-white" ]
        [ div [ class "flex-auto" ] []
        , button
            [ class "btn btn-primary m2"
            , style [ ( "background-color", "darkgreen" ) ]
            , onClick
                <| Outgoing
                <| "bid "
                ++ toString model.quant
                ++ " "
                ++ toString model.rank
            , disabled <| not model.buttons.bfRaise || not (higher model)
            ]
            [ text "Raise" ]
        , div [ class "flex-auto" ] []
        , button
            [ class "btn btn-primary m2"
            , style [ ( "background-color", "darkgreen" ) ]
            , onClick <| Outgoing "challenge"
            , disabled <| not model.buttons.bfChallenge
            ]
            [ text "Challenge" ]
        , div [ class "flex-auto" ] []
        , button
            [ class "btn btn-primary m2"
            , style [ ( "background-color", "darkgreen" ) ]
            , onClick <| Outgoing "count"
            , disabled <| not model.buttons.bfCount
            ]
            [ text "Count" ]
        , div [ class "flex-auto" ] []
        ]


higher : Model -> Bool
higher model =
    let
        mRank =
            if model.rank == 0 then
                10
            else
                model.rank

        cRank =
            if model.bid.bidRank == 0 then
                10
            else
                model.bid.bidRank

        cQuant =
            model.bid.bidQuant
    in
        model.quant > cQuant || (model.quant == cQuant && mRank > cRank)