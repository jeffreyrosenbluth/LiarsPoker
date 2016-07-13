module LiarsPoker.Update exposing (..)

import LiarsPoker.Model exposing (Model, ServerMsg(..), Msg(..), clientMsgDecoder, wsURL, ClientMsg)
import Array exposing (get)
import Json.Decode exposing (..)
import Maybe as M exposing (withDefault)
import WebSocket


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

turn : ClientMsg -> String
turn c =
    withDefault "Error" <| M.map .name <| get c.cmGame.turn c.cmGame.players

bidder : ClientMsg -> String
bidder c =
    let
        b =
            c.cmGame.bidder `M.andThen` \n -> get n c.cmGame.players
    in
        withDefault "None" (M.map .name b)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RaiseRank c ->
            ( { model | card = c }, Cmd.none )

        RaiseQuant q ->
            ( { model | quant = q }, Cmd.none )

        Name n ->
            ( { model | name = n }, Cmd.none )

        GameId g ->
            ( { model | gameId = g }, Cmd.none )

        NumPlayers n ->
            ( { model | numPlayers = n }, Cmd.none )

        WSincoming s ->
            if s == ":signin" then
                ( { model | wsIncoming = RawMsg ":signin" }, Cmd.none )
            else
                case decodeString clientMsgDecoder s of
                    Ok pm ->
                        ( { model
                            | wsIncoming = JsonMsg pm
                            , gameInfo =
                                { name = pm.cmName
                                , turn = turn pm
                                , bidder = bidder pm
                                , baseStake = pm.cmGame.baseStake
                                , multiple = pm.cmMultiple
                                , bid = pm.cmGame.bid
                                , hand = pm.cmHand
                                }
                            , players =
                                { players = pm.cmGame.players
                                , bidder = pm.cmGame.bidder
                                , turn = pm.cmGame.turn
                                }
                          }
                        , Cmd.none
                        )

                    Err e ->
                        ( { model | wsIncoming = ErrorMsg e }, Cmd.none )

        WSoutgoing s ->
            ( model, WebSocket.send wsURL s )

        PlayerList _ ->
            ( model, Cmd.none )
        GameInfo _ ->
            ( model, Cmd.none )
