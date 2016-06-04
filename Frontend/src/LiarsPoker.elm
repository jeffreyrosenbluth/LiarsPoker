module LiarsPoker exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing ( (:=), string, int, andThen, succeed, decodeString
                            , bool, Decoder )
import WebSocket

--------------------------------------------------------------------------------
-- Subscriptions
--------------------------------------------------------------------------------

wsURL : String
wsURL = "ws://localhost:9160/"

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen wsURL WSincoming

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

type alias Model =
  { quant : Int
  , card : Int
  , test : String
  , wsIncoming : String
  }

init : (Model, Cmd Msg)
init =
  ( { quant = 0
    , card = 0
    , test = "Enter command"
    , wsIncoming = ""
    }
    , Cmd.none
  )

type alias PlayerPublic =
  { playerId : Int
  , name : String
  , score : Int
  }

publicPlayer : Decoder PlayerPublic
publicPlayer =
  ( "_pbPlayerId" := int) `andThen` \p1 ->
  ( "_pbName" := string) `andThen` \p2 ->
  ( "_pbScore" := int) `andThen` \p3 ->
  succeed { playerId = p1, name = p2, score = p3 }

type alias PlayerMsg =
  { playersMsg : List PlayerPublic
  , bidderMsg : String
  , bidQuantMsg : Int
  , bidCardMsg : Int
  , turnMsg : String
  , baseStakeMsg : Int
  , multipleMsg : Int
  , myNameMsg : String
  , myHandMsg : String
  , errorMsg : String
  , raiseBtnMsg : Bool
  , chalBtnMsg : Bool
  , countBtnMsg : Bool
  }

playerMsg : Decoder PlayerMsg
playerMsg =
  ("_cmPlayers" := Json.Decode.list publicPlayer) `andThen` \p0 ->
  ("_cmBidder" := string) `andThen` \p1 ->
  ("_cmBidQuant" := int) `andThen` \p2 ->
  ("_cmBidCard" := int) `andThen` \p3 ->
  ("_cmTurn" := string) `andThen` \p4 ->
  ("_cmBaseStake" := int) `andThen` \p5 ->
  ("_cmMultiple" := int) `andThen` \p6 ->
  ("_cmMyName" := string) `andThen` \p7 ->
  ("_cmMyHand" := string) `andThen` \p8 ->
  ("_cmError" := string) `andThen` \p9 ->
  ("_cmRaiseBtn" := bool) `andThen` \p10 ->
  ("_cmChalBtn" := bool) `andThen` \p11 ->
  ("_cmCountBtn" := bool) `andThen` \p12 ->
  succeed { playersMsg = p0
          , bidderMsg = p1
          , bidQuantMsg = p2
          , bidCardMsg = p3
          , turnMsg = p4
          , baseStakeMsg = p5
          , multipleMsg = p6
          , myNameMsg = p7
          , myHandMsg = p8
          , errorMsg = p9
          , raiseBtnMsg = p10
          , chalBtnMsg = p11
          , countBtnMsg = p12
          }

--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

type Msg
  = RaiseRank Int
  | RaiseQuant Int
  | Test String
  | WSincoming String
  | WSoutgoing String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RaiseRank c -> ({ model | card = c}, Cmd.none)
    RaiseQuant q -> ({ model | quant = q}, Cmd.none)
    Test s -> ({ model | test = s}, Cmd.none)
    WSincoming s -> ({ model | wsIncoming = s}, Cmd.none)
    WSoutgoing s -> (model, WebSocket.send wsURL s)

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

view : Model -> Html Msg
view model =
  if model.wsIncoming == ":signin" then
    viewTest model
  else
    case decodeString playerMsg model.wsIncoming of
      Ok pm -> mainView model pm
      Err e -> div [class "h2 p2 m2 red"] [text "Cannot parse server message"]

mainView : Model -> PlayerMsg -> Html Msg
mainView m pm =
  div [ class "flex flex-column m2 border border-box"
      , style [ ("max-width", "40em") ]
      ]
      [ div [ class "flex-justify border border-box"] [currentPlayerView pm, handView pm ]
      , div [ class "flex bg-white"]
          [ div [ style [("width", "50%")]] [bidderView pm]
          , div [ style [("width", "50%")]] [playerView pm]
          ]
      , div [ class "flex bg-white"]
          [ div [ style [("width", "50%")]] [stakesView pm]
          , div [ style [("width", "50%")]] [multipleView pm]
          ]
      , div [ class "bg-white"] [bidView pm]
      , div [ class "flex bg-white border-box border h2"]
          [ playerListView pm
          , scoreListView pm
          ]
      , quantEntryView m
      , rankEntryView m
      , playView m pm
      , div [ class "p1 center red"] [text pm.errorMsg]
      , if pm.myHandMsg == "" then viewTest m else div [] []
      ]

currentPlayerView : PlayerMsg -> Html Msg
currentPlayerView pm =
  div [class "center mt2 h2", style [("color", "#3CA962")]] [text pm.myNameMsg]

handView : PlayerMsg -> Html Msg
handView pm =
  div [class "center p2 h1 bold", style [("color", "#3CA962")]] [text pm.myHandMsg]

bidderView : PlayerMsg -> Html Msg
bidderView pm =
  div [class "flex bg-white"]
    [ div [class "ml1 p1 h2 gray"] [text "Bidder"]
    , div [class "p1 h2"] [text  pm.bidderMsg]
    ]

bidView : PlayerMsg -> Html Msg
bidView pm =
  div [class "flex bold", style [("background-color", "gainsboro")]]
    [ div [class "flex-auto"] []
    , div [class "p1 h2", style [("color", "navy")]] [text "Bid"]
    , div [class "p1 h2", style [("color", "navy")]]
          [text  <| toString pm.bidQuantMsg ++ " "
                 ++ toString pm.bidCardMsg ++ "s"]
    , div [class "flex-auto"] []
    ]

stakesView : PlayerMsg -> Html Msg
stakesView pm =
  div [class "flex bg-white"]
    [ div [class "ml1 p1 h2 gray"] [text "Base Stake"]
    , div [class "p1 h2"] [text <| toString pm.baseStakeMsg]
    ]

multipleView : PlayerMsg -> Html Msg
multipleView pm =
  div [class "flex bg-white"]
    [ div [class "p1 h2 gray"] [text "Multiple"]
    , div [class "p1 h2"] [text <| toString  pm.multipleMsg]
    ]

playerView : PlayerMsg -> Html Msg
playerView pm =
  div [class "flex bg-white"]
    [ div [class "p1 h2 gray"] [text "Current"]
    , div [class "p1 h2"] [text pm.turnMsg]
    ]

playerListView : PlayerMsg -> Html Msg
playerListView pm =
  let
    sty x =
      if x == pm.bidderMsg
        then [style [("color", "firebrick")]]
      else if x == pm.turnMsg
        then [style [("color", "darkblue")]]
      else [style [("color", "gray")]]
    ps = List.map (\x -> li (sty x) [text x]) (List.map .name pm.playersMsg)
  in
    ul [class "list-reset ml2 mt1", style [("width", "70%")]] ps

scoreListView : PlayerMsg -> Html Msg
scoreListView pm =
  let
    ss = List.map (\x -> li [] [text x])
                  (List.map (toString << .score) pm.playersMsg)
  in
    ul [class "list-reset mt1", style [("width", "30%")]] ss

quantEntryView : Model -> Html Msg
quantEntryView m =
  div [class "flex bg-white"]
    [ div [class "h2 ml4 p1 gray"] [text "Quantity"]
    , div [class "flex-auto"] []
    , div [class "h2 p1"] [text <| toString m.quant]
    , div [class "flex-auto"] []
    , button [ class "btn btn-outline m1 h6"
             , onClick (RaiseQuant <| Basics.max 0 (m.quant - 1))
             ]
             [ text "-" ]
    , button [ class "btn btn-outline mt1 mb1 mr4 h6"
             , onClick (RaiseQuant <| m.quant + 1)
             ]
             [ text "+" ]
    ]

constrainRank : Int -> Int
constrainRank n =
  if n == 10
    then 0
  else if n < 0
    then 9
  else n

rankEntryView : Model -> Html Msg
rankEntryView m =
  div [class "flex bg-white"]
    [ div [class "h2 ml4 p1 gray"] [text "Rank"]
    , div [class "flex-auto"] []
    , div [class "h2 p1 ml3"] [text <| toString m.card]
    , div [class "flex-auto"] []
    , button [ class "btn btn-outline m1 h6"
             , onClick <| RaiseRank <| constrainRank <| m.card - 1
             ]
             [ text "-" ]
    , button [ class "btn btn-outline mt1 mb1 mr4 h6"
             , onClick <| RaiseRank <| constrainRank <| m.card + 1
             ]
             [ text "+" ]
    ]

playView : Model -> PlayerMsg -> Html Msg
playView m pm =
  div [class "flex bg-white"]
    [ div [class "flex-auto"] []
    , button [ class "btn btn-primary bg-gray black m2"
             , onClick <| WSoutgoing
                       <| "bid " ++ toString  m.quant
                                 ++ " "
                                 ++ toString  m.card
             , disabled <| not pm.raiseBtnMsg
             ]
             [ text "Raise" ]
    , div [class "flex-auto"] []
    , button [ class "btn btn-primary bg-gray black m2"
             , onClick <| WSoutgoing "challenge"
             , disabled <| not pm.chalBtnMsg
             ]
             [text "Challenge"]
    , div [class "flex-auto"] []
    , button [ class "btn btn-primary bg-gray black m2"
             , onClick <| WSoutgoing "count"
             , disabled <| not pm.countBtnMsg
             ]
             [text "Count"]
    , div [class "flex-auto"] []
    ]

viewTest : Model -> Html Msg
viewTest model =
  div [ class "flex p2 m2 border"]
    [ input
        [ placeholder "Enter Command"
        , value model.test
        , onInput Test
        , class "p1 center"
        , style [("width", "150px")]
        ]
        []
    , button [class "ml3 btn btn-primary bg-gray h3", onClick <| WSoutgoing model.test]
             [text "Submit"]
    ]
