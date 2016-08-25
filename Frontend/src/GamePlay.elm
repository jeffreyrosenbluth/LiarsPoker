module GamePlay exposing (..)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (toInt)
import WebSocket


type alias Model =
    { quant : Int
    , rank : Int
    , buttons : BtnFlags
    , bid : Bid
    , preResult : Bool
    }


type Msg
    = RaiseQuant Int
    | RaiseRank Int
    | Outgoing String
    | PreResult Bool


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
    , preResult = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RaiseQuant q ->
            ( { model | quant = q }, Cmd.none )

        RaiseRank r ->
            ( { model | rank = r }, Cmd.none )

        Outgoing s ->
            ( { model | preResult = s == "count" }, WebSocket.send wsURL s )

        PreResult b ->
            ( { model | preResult = b }, Cmd.none )


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
        [ div [ class "h2 flex-auto p1 gray", style [ ( "width", "5rem" ) ] ] [ text "Quantity" ]
        , div [ class "flex-auto" ] []
        , input
            [ Html.Attributes.value (toString model.quant)
            , onInput
                <| toInt
                >> Result.toMaybe
                >> Maybe.withDefault 0
                >> Basics.max 0
                >> RaiseQuant
            , class "m1 center field"
            , type' "number"
            , style [ ( "font-size", "120%" ) ]
            ]
            []
        , div [ class "flex-auto" ] []
        ]


constrainRank : Int -> Int
constrainRank n =
    if n >= 10 then
        0
    else if n < 0 then
        9
    else
        n


rankEntryView : Model -> Html Msg
rankEntryView model =
    div [ class "flex bg-white" ]
        [ div [ class "h2 flex-auto p1 gray", style [ ( "width", "5rem" ) ] ] [ text "Rank" ]
        , div [ class "flex-auto" ] []
        , input
            [ Html.Attributes.value (toString model.rank)
            , onInput
                <| toInt
                >> Result.toMaybe
                >> Maybe.withDefault 0
                >> constrainRank
                >> RaiseRank
            , class "h2 m1 center field"
            , type' "number"
            , style [ ( "font-size", "120%" ) ]
            ]
            []
        , div [ class "flex-auto" ] []
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
