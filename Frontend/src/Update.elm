module Update exposing (..)

{-| Top level updates
-}

import Model exposing (..)
import GamePlay as GamePlay
import SignIn as SignIn
import Array exposing (get)
import Maybe as M exposing (withDefault)
import Json.Decode exposing (..)


{-| A utility function for extracting the name of the player whose turn it is.
-}
turn : ClientMsg -> String
turn c =
    withDefault "Error" <| M.map .name <| get c.cmGame.turn c.cmGame.players


{-| A utility function for extracting the name of the player whose is the current
    bidder. Returns "None" if a bid has not been made yet.
-}
bidder : ClientMsg -> String
bidder c =
    let
        b =
            c.cmGame.bidder `M.andThen` \n -> get n c.cmGame.players
    in
        withDefault "None" (M.map .name b)


{-| Most of the interesting action happens in the WSincoming case where a
    message string comes in from the servers over a websocket. If the message
    does not start with ":signin" then we assume it is JSON. We then decode it
    and update the model accordingly.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GamePlay m ->
            let
                ( gp, cmd ) =
                    GamePlay.update m model.gamePlay
            in
                ( Model model.wsIncoming model.players model.gameInfo gp model.signIn
                , Cmd.map GamePlay cmd
                )

        SignIn m ->
            let
                ( si, cmd ) =
                    SignIn.update m model.signIn
            in
                ( Model model.wsIncoming model.players model.gameInfo model.gamePlay  si
                , Cmd.map SignIn cmd
                )

        WSincoming s ->
            if s == ":signin" then
                ( { model | wsIncoming = RawMsg ":signin" }, Cmd.none )
            else
                case decodeString clientMsgDecoder s of
                    Ok pm ->
                        ( updateCM pm model
                        , Cmd.none
                        )

                    Err e ->
                        ( { model | wsIncoming = ErrorMsg e }, Cmd.none )

        None ->
            ( model, Cmd.none )


{-| Helper method to update the model when the Msg is an wsIncoming.
-}
updateCM : ClientMsg -> Model -> Model
updateCM cMsg model =
    let
        q =
            model.gamePlay.quant

        r =
            model.gamePlay.rank
    in
        { model
            | wsIncoming = JsonMsg cMsg
            , gameInfo =
                { name = cMsg.cmName
                , turn = turn cMsg
                , bidder = bidder cMsg
                , baseStake = cMsg.cmGame.baseStake
                , multiple = cMsg.cmMultiple
                , bid = cMsg.cmGame.bid
                , hand = cMsg.cmHand
                }
            , players =
                { players = cMsg.cmGame.players
                , bidder = cMsg.cmGame.bidder
                , turn = cMsg.cmGame.turn
                }
            , gamePlay =
                { quant = q
                , rank = r
                , buttons = cMsg.cmButtons
                , bid = cMsg.cmGame.bid
                , preResult = model.gamePlay.preResult
                }
        }
