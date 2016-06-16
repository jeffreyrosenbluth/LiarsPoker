module LiarsPoker.View exposing (..)

import LiarsPoker.Model exposing (Model, Msg(..), ServerMsg(..), showServerMsg, ClientMsg, higher)
import Array as A exposing (Array, get)
import Maybe as M exposing (withDefault)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    case model.wsIncoming of
        RawMsg ":signin" ->
            viewTest model

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
        , style [ ( "max-width", "40em" ) ]
        ]
        [ div [ class "flex-justify border border-box" ] [ currentPlayerView c, handView c ]
        , div [ class "flex bg-white" ]
            [ div [ style [ ( "width", "50%" ) ] ] [ bidderView c ]
            , div [ style [ ( "width", "50%" ) ] ] [ playerView c ]
            ]
        , div [ class "flex bg-white" ]
            [ div [ style [ ( "width", "50%" ) ] ] [ stakesView c ]
            , div [ style [ ( "width", "50%" ) ] ] [ multipleView c ]
            ]
        , div [ class "bg-white" ] [ bidView c ]
        , div [ class "flex bg-white border-box border h2" ]
            [ playerListView c
            , scoreListView c
            ]
        , quantEntryView m
        , rankEntryView m
        , playView m c
        , div [ class "p1 center red" ] [ text <| showServerMsg m.wsIncoming ]
        , if c.cmHand == "" then
            viewTest m
          else
            div [] []
        ]


currentPlayerView : ClientMsg -> Html Msg
currentPlayerView c =
    div [ class "center mt2 h2", style [ ( "color", "#3CA962" ) ] ] [ text c.cmName ]


handView : ClientMsg -> Html Msg
handView c =
    div [ class "center p2 h1 bold", style [ ( "color", "#3CA962" ) ] ] [ text c.cmHand ]


bidder : ClientMsg -> String
bidder c =
    let
        b =
            c.cmGame.bidder `M.andThen` \n -> get n c.cmGame.players
    in
        withDefault "" (M.map .name b)


bidderView : ClientMsg -> Html Msg
bidderView c =
    let
        b =
            bidder c
    in
        div [ class "flex bg-white" ]
            [ div [ class "ml1 p1 h2 gray" ] [ text "Bidder" ]
            , div [ class "p1 h2" ] [ text b ]
            ]


bidView : ClientMsg -> Html Msg
bidView c =
    div [ class "flex bold", style [ ( "background-color", "gainsboro" ) ] ]
        [ div [ class "flex-auto" ] []
        , div [ class "p1 h2", style [ ( "color", "navy" ) ] ] [ text "Bid" ]
        , div [ class "p1 h2", style [ ( "color", "navy" ) ] ]
            [ text
                <| toString c.cmGame.bid.bidQuant
                ++ " "
                ++ toString c.cmGame.bid.bidCard
                ++ "s"
            ]
        , div [ class "flex-auto" ] []
        ]


stakesView : ClientMsg -> Html Msg
stakesView c =
    div [ class "flex bg-white" ]
        [ div [ class "ml1 p1 h2 gray" ] [ text "Base Stake" ]
        , div [ class "p1 h2" ] [ text <| toString c.cmGame.baseStake ]
        ]


multipleView : ClientMsg -> Html Msg
multipleView c =
    div [ class "flex bg-white" ]
        [ div [ class "p1 h2 gray" ] [ text "Multiple" ]
        , div [ class "p1 h2" ] [ text <| toString c.cmMultiple ]
        ]


turn : ClientMsg -> String
turn c =
    withDefault "Error" <| M.map .name <| get c.cmGame.turn c.cmGame.players


playerView : ClientMsg -> Html Msg
playerView c =
    div [ class "flex bg-white" ]
        [ div [ class "p1 h2 gray" ] [ text "Current" ]
        , div [ class "p1 h2" ] [ text <| turn c ]
        ]


playerListView : ClientMsg -> Html Msg
playerListView c =
    let
        sty x =
            if x == bidder c then
                [ style [ ( "color", "firebrick" ) ] ]
            else if x == turn c then
                [ style [ ( "color", "darkblue" ) ] ]
            else
                [ style [ ( "color", "gray" ) ] ]

        ps =
            A.toList <| A.map (\x -> li (sty x) [ text x ]) (A.map .name c.cmGame.players)
    in
        ul [ class "list-reset ml2 mt1", style [ ( "width", "70%" ) ] ] ps


scoreListView : ClientMsg -> Html Msg
scoreListView c =
    let
        ss =
            A.toList
                <| A.map (\x -> li [] [ text x ])
                    (A.map (toString << .score) c.cmGame.players)
    in
        ul [ class "list-reset mt1", style [ ( "width", "30%" ) ] ] ss


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
            [ text "-" ]
        , button
            [ class "btn btn-outline mt1 mb1 mr4 h6"
            , onClick (RaiseQuant <| m.quant + 1)
            ]
            [ text "+" ]
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
            [ text "-" ]
        , button
            [ class "btn btn-outline mt1 mb1 mr4 h6"
            , onClick <| RaiseRank <| constrainRank <| m.card + 1
            ]
            [ text "+" ]
        ]


playView : Model -> ClientMsg -> Html Msg
playView m c =
    div [ class "flex bg-white" ]
        [ div [ class "flex-auto" ] []
        , button
            [ class "btn btn-primary bg-gray black m2"
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
            [ class "btn btn-primary bg-gray black m2"
            , onClick <| WSoutgoing "challenge"
            , disabled <| not c.cmButtons.bfChallenge
            ]
            [ text "Challenge" ]
        , div [ class "flex-auto" ] []
        , button
            [ class "btn btn-primary bg-gray black m2"
            , onClick <| WSoutgoing "count"
            , disabled <| not c.cmButtons.bfCount
            ]
            [ text "Count" ]
        , div [ class "flex-auto" ] []
        ]


viewTest : Model -> Html Msg
viewTest model =
    div [ class "flex p2 m2 border" ]
        [ input
            [ placeholder "Enter Command"
            , Html.Attributes.value model.test
            , onInput Test
            , class "p1 center"
            , style [ ( "width", "150px" ) ]
            ]
            []
        , button
            [ class "ml3 btn btn-primary bg-gray h3"
            , onClick <| WSoutgoing model.test
            ]
            [ text "Submit" ]
        ]
