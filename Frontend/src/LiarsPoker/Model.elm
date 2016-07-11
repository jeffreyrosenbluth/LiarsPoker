module LiarsPoker.Model exposing (..)

import Array as A exposing (Array, get)
import Json.Decode exposing (..)
import WebSocket


--------------------------------------------------------------------------------
-- Subscriptions
--------------------------------------------------------------------------------


wsURL : String
wsURL =
    -- "wss://liarspoker.herokuapp.com"
    "ws://localhost:9160/"


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen wsURL WSincoming



--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type Msg
    = RaiseRank Int
    | RaiseQuant Int
    | Name String
    | GameId String
    | NumPlayers Int
    | WSincoming String
    | WSoutgoing String


type ServerMsg
    = RawMsg String
    | JsonMsg ClientMsg
    | ErrorMsg String


showServerMsg : ServerMsg -> String
showServerMsg sm =
    case sm of
        RawMsg s ->
            s

        JsonMsg cm ->
            cm.cmError

        ErrorMsg e ->
            e


type alias Model =
    { name : String
    , gameId : String
    , numPlayers : Int
    , quant : Int
    , card : Int
    , wsIncoming : ServerMsg
    }


init : ( Model, Cmd Msg )
init =
    ( { name = ""
      , gameId = "0"
      , numPlayers = 0
      , quant = 0
      , card = 0
      , wsIncoming = RawMsg ""
      }
    , Cmd.none
    )


type alias Bid =
    { bidCard : Int
    , bidQuant : Int
    }


(<*>) : Decoder (a -> b) -> Decoder a -> Decoder b
(<*>) =
    object2 (<|)


bidDecoder : Decoder Bid
bidDecoder =
    succeed Bid
        <*> ("_bidCard" := int)
        <*> ("_bidQuant" := int)


type alias Player =
    { playerId : Int
    , name : String
    , score : Int
    }


playerDecoder : Decoder Player
playerDecoder =
    succeed Player
        <*> ("_playerId" := int)
        <*> ("_name" := string)
        <*> ("_score" := int)


type alias Game =
    { players : Array Player
    , bidder : Maybe Int
    , bid : Bid
    , turn : Int
    , won : Maybe Bool
    , rebid : Bool
    , inProgress : Bool
    , baseStake : Int
    , gameId : Int
    }


gameDecoder : Decoder Game
gameDecoder =
    succeed Game
        <*> ("_players" := array playerDecoder)
        <*> (maybe ("_bidder" := int))
        <*> ("_bid" := bidDecoder)
        <*> ("_turn" := int)
        <*> (maybe ("_won" := bool))
        <*> ("_rebid" := bool)
        <*> ("_inProgress" := bool)
        <*> ("_baseStake" := int)
        <*> ("_gameId" := int)


type alias BtnFlags =
    { bfRaise : Bool
    , bfChallenge : Bool
    , bfCount : Bool
    }


btnFlagsDecoder : Decoder BtnFlags
btnFlagsDecoder =
    succeed BtnFlags
        <*> ("_bfRaise" := bool)
        <*> ("_bfChallenge" := bool)
        <*> ("_bfCount" := bool)

type alias PrevGame =
  { pgBidder : String
  , pgBid : Bid
  , pgCount : Int
  , pgMe : Array Int
  }

prevGameDecoder : Decoder PrevGame
prevGameDecoder =
  succeed PrevGame
    <*> ("_pgBidder" := string)
    <*> ("_pgBid" := bidDecoder)
    <*> ("_pgCount" := int)
    <*> ("_pgMe" := array int)

type alias ClientMsg =
    { cmGame : Game
    , cmHand : String
    , cmError : String
    , cmMultiple : Int
    , cmButtons : BtnFlags
    , cmName : String
    , cmPrevGame : PrevGame
    , cmPlyrId : Int
    }


clientMsgDecoder : Decoder ClientMsg
clientMsgDecoder =
    succeed ClientMsg
        <*> ("_cmGame" := gameDecoder)
        <*> ("_cmHand" := string)
        <*> ("_cmError" := string)
        <*> ("_cmMultiple" := int)
        <*> ("_cmButtons" := btnFlagsDecoder)
        <*> ("_cmName" := string)
        <*> ("_cmPrevGame" := prevGameDecoder)
        <*> ("_cmPlyrId" := int)


higher : Model -> ClientMsg -> Bool
higher m c =
    let
        mCard =
            if m.card == 0 then
                10
            else
                m.card

        cCard =
            if c.cmGame.bid.bidCard == 0 then
                10
            else
                c.cmGame.bid.bidCard

        cQuant =
            c.cmGame.bid.bidQuant
    in
        m.quant > cQuant || (m.quant == cQuant && mCard > cCard)
