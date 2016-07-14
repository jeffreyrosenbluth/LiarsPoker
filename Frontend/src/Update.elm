module Update exposing (..)

import Model exposing (..)
import GamePlay as GamePlay
import SignIn as SignIn
import Array exposing (get)
import Maybe as M exposing (withDefault)
import Json.Decode exposing (..)


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
                ( Model model.wsIncoming model.players model.gameInfo model.gamePlay si
                , Cmd.map SignIn cmd
                )

        WSincoming s ->
            if s == ":signin" then
                ( { model | wsIncoming = RawMsg ":signin" }, Cmd.none )
            else
                case decodeString clientMsgDecoder s of
                    Ok pm ->
                        ( updateModel model pm
                        , Cmd.none
                        )

                    Err e ->
                        ( { model | wsIncoming = ErrorMsg e }, Cmd.none )

        PlayerList _ ->
            ( model, Cmd.none )

        GameInfo _ ->
            ( model, Cmd.none )


updateModel : Model -> ClientMsg -> Model
updateModel model pm =
    let
        q =
            model.gamePlay.quant

        r =
            model.gamePlay.rank
    in
        { model
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
            , gamePlay =
                { quant = q
                , rank = r
                , buttons = pm.cmButtons
                , bid = pm.cmGame.bid
                }
        }
