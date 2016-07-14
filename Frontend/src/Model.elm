module Model exposing (..)

import Types exposing (..)
import PlayerList as PlayerList
import GameInfo as GameInfo
import GamePlay as GamePlay
import SignIn as SignIn
import Array as A exposing (Array, get)
import Json.Decode exposing (..)
import WebSocket


--------------------------------------------------------------------------------
-- Subscriptions
--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen wsURL WSincoming


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type Msg
    = WSincoming String
    | GameInfo ()
    | PlayerList ()
    | GamePlay GamePlay.Msg
    | SignIn SignIn.Msg


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
    { wsIncoming : ServerMsg
    , players : PlayerList.Model
    , gameInfo : GameInfo.Model
    , gamePlay : GamePlay.Model
    , signIn : SignIn.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { wsIncoming = RawMsg ""
      , players = PlayerList.init
      , gameInfo = GameInfo.init
      , gamePlay = GamePlay.init
      , signIn = SignIn.init
      }
    , Cmd.none
    )


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
            if m.gamePlay.rank == 0 then
                10
            else
                m.gamePlay.rank

        cCard =
            if c.cmGame.bid.bidCard == 0 then
                10
            else
                c.cmGame.bid.bidCard

        cQuant =
            c.cmGame.bid.bidQuant
    in
        m.gamePlay.quant > cQuant || (m.gamePlay.quant == cQuant && mCard > cCard)
