module LiarsPoker.Player exposing (..)

import Json.Decode exposing (..)


(<*>) : Decoder (a -> b) -> Decoder a -> Decoder b
(<*>) =
    object2 (<|)

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
