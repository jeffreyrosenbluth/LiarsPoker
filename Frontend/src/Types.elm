module Types exposing (..)

import Json.Decode exposing (..)


wsURL : String
wsURL =
    "wss://liarspoker.herokuapp.com"



-- "ws://localhost:9160/"


(<*>) : Decoder (a -> b) -> Decoder a -> Decoder b
(<*>) =
    object2 (<|)


type alias Bid =
    { bidRank : Int
    , bidQuant : Int
    }


bidDecoder : Decoder Bid
bidDecoder =
    succeed Bid
        <*> ("_bidRank" := int)
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
