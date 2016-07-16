module Model exposing (..)

{-| Top level model and decoders for communicating with the LiarsPoker
    Websockets sever
-}

import Types exposing (..)
import PlayerList as PlayerList
import GameInfo as GameInfo
import GamePlay as GamePlay
import SignIn as SignIn
import Array as A exposing (Array, get)
import Json.Decode exposing (..)
import WebSocket


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen wsURL WSincoming


{-| There are only 3 non-trivial message types. WSincoming for websocket strings
    coming from the server. GamePlay for internal messages needed to update the
    game play page UI and SignIn messages to update the sign in page UI.
-}
type Msg
    = WSincoming String
    | GamePlay GamePlay.Msg
    | SignIn SignIn.Msg
    | None ()


{-| wsIncoming tracks input from the server, players is the component with
    player names and scores (no user interaction and hence no associated messages),
    gameInfo is a component that displays the rest of the static game data.
    gamePlay is the component that handles player actions and signIn for strarting
    or joining a game.
-}
type alias Model =
    { wsIncoming : ServerMsg
    , players : PlayerList.Model
    , gameInfo : GameInfo.Model
    , gamePlay : GamePlay.Model
    , signIn : SignIn.Model
    }


{-| Messages from the game server can be raw strings, json, or errors.
-}
type ServerMsg
    = RawMsg String
    | JsonMsg ClientMsg
    | ErrorMsg String


{-| Show either the raw server message string or an error string.
-}
showServerMsg : ServerMsg -> String
showServerMsg sm =
    case sm of
        RawMsg s ->
            s

        JsonMsg cm ->
            cm.cmError

        ErrorMsg e ->
            e


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


{-| Matches the Game data type from the server so that serialized messages
    can be "reconstituted to the same type."
-}
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
    , numPlyrs : Int
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
        <*> ("_numPlyrs" := int)


{-| Matches the PrevGame data type from the server to show results after a count.
-}
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


{-| We parse a serialized JSON string from the server into a ClientMsg.
-}
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


{-| Since 0s are interpreted as 10s we need a function to compare ranks.
-}
higher : Model -> ClientMsg -> Bool
higher m c =
    let
        mRank =
            if m.gamePlay.rank == 0 then
                10
            else
                m.gamePlay.rank

        cRank =
            if c.cmGame.bid.bidRank == 0 then
                10
            else
                c.cmGame.bid.bidRank

        cQuant =
            c.cmGame.bid.bidQuant
    in
        m.gamePlay.quant > cQuant || (m.gamePlay.quant == cQuant && mRank > cRank)
