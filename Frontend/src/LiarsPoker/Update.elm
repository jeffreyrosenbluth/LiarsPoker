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

        Test s ->
            ( { model | test = s }, Cmd.none )

        WSincoming s ->
            let
                m =
                    if s == ":signin" then
                        RawMsg ":signin"
                    else
                        case decodeString clientMsgDecoder s of
                            Ok pm ->
                                JsonMsg pm

                            Err e ->
                                ErrorMsg e
            in
                ( { model | wsIncoming = m }, Cmd.none )

        WSoutgoing s ->
            ( model, WebSocket.send wsURL s )
