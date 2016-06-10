module LiarsPoker exposing (..)

import Array as A exposing (Array, get)
import Maybe as M exposing (withDefault)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (..)
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

type alias Bid =
  { bidCard : Int
  , bidQuant : Int
  }

bid : Decoder Bid
bid =
  ( "_bidCard" := int) `andThen` \p0 ->
  ( "_bidQuant" := int) `andThen` \p1 ->
  succeed { bidCard = p0, bidQuant = p1 }

type alias Player =
  { playerId : Int
  , name : String
  , score : Int
  }

player : Decoder Player
player =
  ( "_playerId" := int) `andThen` \p1 ->
  ( "_name" := string) `andThen` \p2 ->
  ( "_score" := int) `andThen` \p3 ->
  succeed { playerId = p1, name = p2, score = p3 }

type alias Game =
  { players : Array Player
  , bidder : Maybe Int
  , bid : Bid
  , turn : Int
  , won : Maybe Bool
  , rebid : Bool
  , inProgress : Bool
  , baseStake : Int
  }

game : Decoder Game
game =
  ( "_players" := array player) `andThen` \p0 ->
  ( maybe ("_bidder" := int)) `andThen` \p1 ->
  ( "_bid" := bid) `andThen` \p2 ->
  ( "_turn" := int) `andThen` \p3 ->
  ( maybe ("_won" := bool)) `andThen` \p4 ->
  ( "_rebid" := bool) `andThen` \p5 ->
  ( "_inProgress" := bool) `andThen` \p6 ->
  ( "_baseStake" := int) `andThen` \p7 ->
  succeed { players = p0
         , bidder = p1
         , bid = p2
         , turn = p3
         , won = p4
         , rebid = p5
         , inProgress = p6
         , baseStake = p7
         }

type alias BtnFlags =
  { bfRaise : Bool
  , bfChallenge : Bool
  , bfCount : Bool
  }

btnFlags : Decoder BtnFlags
btnFlags =
  ( "_bfRaise" := bool) `andThen` \p0 ->
  ( "_bfChallenge" := bool) `andThen` \p1 ->
  ( "_bfCount" := bool) `andThen` \p2 ->
  succeed { bfRaise = p0, bfChallenge = p1, bfCount = p2 }

type alias ClientMsg =
  { cmGame : Game
  , cmHand : String
  , cmError : String
  , cmMultiple : Int
  , cmButtons : BtnFlags
  , cmName : String
  }

clientMsg : Decoder ClientMsg
clientMsg =
  ("_cmGame" := game) `andThen` \p0 ->
  ("_cmHand" := string) `andThen` \p1 ->
  ("_cmError" := string) `andThen` \p2 ->
  ("_cmMultiple" := int) `andThen` \p3 ->
  ("_cmButtons" := btnFlags) `andThen` \p4 ->
  ("_cmName" := string) `andThen` \p5 ->
  succeed { cmGame = p0
          , cmHand = p1
          , cmError = p2
          , cmMultiple = p3
          , cmButtons = p4
          , cmName = p5
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
    case decodeString clientMsg model.wsIncoming of
      Ok pm -> mainView model pm
      Err e -> div [class "h2 p2 m2 red"] [text "Cannot parse server message"]

mainView : Model -> ClientMsg -> Html Msg
mainView m c =
  div [ class "flex flex-column m2 border border-box"
      , style [ ("max-width", "40em") ]
      ]
      [ div [ class "flex-justify border border-box"] [currentPlayerView c, handView c ]
      , div [ class "flex bg-white"]
          [ div [ style [("width", "50%")]] [bidderView c]
          , div [ style [("width", "50%")]] [playerView c]
          ]
      , div [ class "flex bg-white"]
          [ div [ style [("width", "50%")]] [stakesView c]
          , div [ style [("width", "50%")]] [multipleView c]
          ]
      , div [ class "bg-white"] [bidView c]
      , div [ class "flex bg-white border-box border h2"]
          [ playerListView c
          , scoreListView c
          ]
      , quantEntryView m
      , rankEntryView m
      , playView m c
      , div [ class "p1 center red"] [text c.cmError]
      , if c.cmHand == "" then viewTest m else div [] []
      ]

currentPlayerView : ClientMsg -> Html Msg
currentPlayerView c =
  div [class "center mt2 h2", style [("color", "#3CA962")]] [text c.cmName]

handView : ClientMsg -> Html Msg
handView c =
  div [class "center p2 h1 bold", style [("color", "#3CA962")]] [text c.cmHand]

bidder : ClientMsg -> String
bidder c =
  let b = c.cmGame.bidder `M.andThen` \n -> get n c.cmGame.players
  in  withDefault  "" (M.map .name b)

bidderView : ClientMsg -> Html Msg
bidderView c =
  let b = bidder c
  in  div [class "flex bg-white"]
        [ div [class "ml1 p1 h2 gray"] [text "Bidder"]
        , div [class "p1 h2"] [text  b]
        ]

bidView : ClientMsg -> Html Msg
bidView c =
  div [class "flex bold", style [("background-color", "gainsboro")]]
    [ div [class "flex-auto"] []
    , div [class "p1 h2", style [("color", "navy")]] [text "Bid"]
    , div [class "p1 h2", style [("color", "navy")]]
          [text  <| toString c.cmGame.bid.bidQuant ++ " "
                 ++ toString c.cmGame.bid.bidCard ++ "s"]
    , div [class "flex-auto"] []
    ]

stakesView : ClientMsg -> Html Msg
stakesView c =
  div [class "flex bg-white"]
    [ div [class "ml1 p1 h2 gray"] [text "Base Stake"]
    , div [class "p1 h2"] [text <| toString c.cmGame.baseStake]
    ]

multipleView : ClientMsg -> Html Msg
multipleView c =
  div [class "flex bg-white"]
    [ div [class "p1 h2 gray"] [text "Multiple"]
    , div [class "p1 h2"] [text <| toString  c.cmMultiple]
    ]

turn : ClientMsg -> String
turn c = withDefault "Error" <| M.map .name <| get c.cmGame.turn c.cmGame.players

playerView : ClientMsg -> Html Msg
playerView c =
  div [class "flex bg-white"]
    [ div [class "p1 h2 gray"] [text "Current"]
    , div [class "p1 h2"] [text <| turn c]
    ]

playerListView : ClientMsg -> Html Msg
playerListView c =
  let
    sty x =
      if x == bidder c
        then [style [("color", "firebrick")]]
      else if x == turn c
        then [style [("color", "darkblue")]]
      else [style [("color", "gray")]]
    ps = A.toList <| A.map (\x -> li (sty x) [text x]) (A.map .name c.cmGame.players)
  in
    ul [class "list-reset ml2 mt1", style [("width", "70%")]] ps

scoreListView : ClientMsg -> Html Msg
scoreListView c =
  let
    ss = A.toList <| A.map (\x -> li [] [text x])
                   ( A.map (toString << .score) c.cmGame.players)
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

playView : Model -> ClientMsg -> Html Msg
playView m c =
  div [class "flex bg-white"]
    [ div [class "flex-auto"] []
    , button [ class "btn btn-primary bg-gray black m2"
             , onClick <| WSoutgoing
                       <| "bid " ++ toString  m.quant
                                 ++ " "
                                 ++ toString  m.card
             , disabled <| not c.cmButtons.bfRaise
             ]
             [ text "Raise" ]
    , div [class "flex-auto"] []
    , button [ class "btn btn-primary bg-gray black m2"
             , onClick <| WSoutgoing "challenge"
             , disabled <| not c.cmButtons.bfChallenge
             ]
             [text "Challenge"]
    , div [class "flex-auto"] []
    , button [ class "btn btn-primary bg-gray black m2"
             , onClick <| WSoutgoing "count"
             , disabled <| not c.cmButtons.bfCount
             ]
             [text "Count"]
    , div [class "flex-auto"] []
    ]

viewTest : Model -> Html Msg
viewTest model =
  div [ class "flex p2 m2 border"]
    [ input
        [ placeholder "Enter Command"
        , Html.Attributes.value model.test
        , onInput Test
        , class "p1 center"
        , style [("width", "150px")]
        ]
        []
    , button [class "ml3 btn btn-primary bg-gray h3", onClick <| WSoutgoing model.test]
             [text "Submit"]
    ]
