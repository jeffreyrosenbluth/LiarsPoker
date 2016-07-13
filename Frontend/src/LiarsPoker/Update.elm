module LiarsPoker.Update exposing (..)

import LiarsPoker.Model exposing (Model, ServerMsg(..), Msg(..), clientMsgDecoder, wsURL)
import Json.Decode exposing (..)
import WebSocket


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


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
