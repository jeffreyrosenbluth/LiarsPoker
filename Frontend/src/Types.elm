module Types exposing (..)

import Json.Decode exposing (..)


wsURL : String
wsURL =
    -- "wss://liarspoker.herokuapp.com"
    "ws://localhost:9160/"


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


type alias Flags =
    { raiseFlag : Bool
    , chalFlag : Bool
    , countFlag : Bool
    , dealFlag : Bool
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    succeed Flags
        <*> ("_raiseFlag" := bool)
        <*> ("_chalFlag" := bool)
        <*> ("_countFlag" := bool)
        <*> ("_dealFlag" := bool)


type alias Player =
    { playerId : Int
    , name : String
    , score : Int
    , flags : Flags
    }


playerDecoder : Decoder Player
playerDecoder =
    succeed Player
        <*> ("_playerId" := int)
        <*> ("_name" := string)
        <*> ("_score" := int)
        <*> ("_flags" := flagsDecoder)
