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
turn : Game -> String
turn g =
    withDefault "Error" <| M.map .name <| get g.turn g.players


{-| A utility function for extracting the name of the player whose is the current
    bidder. Returns "None" if a bid has not been made yet.
-}
bidder : Game -> String
bidder g =
    let
        b =
            g.bidder `M.andThen` \n -> get n g.players
    in
        withDefault "" (M.map .name b)


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
                ( Model model.wsIncoming model.players model.gameInfo model.gamePlay si
                , Cmd.map SignIn cmd
                )

        WSincoming s ->
            if s == ":signin" then
                ( { model | wsIncoming = RawMsg ":signin" }, Cmd.none )
            else
                case decodeString resultGameDecoder s of
                    Ok pm ->
                        case pm of
                            Ok p ->
                                ( updateCM p model
                                , Cmd.none
                                )
                            Err e ->
                                ( { model | wsIncoming = ErrorMsg e }, Cmd.none )

                    Err e ->
                        ( { model | wsIncoming = ErrorMsg e }, Cmd.none )

        None ->
            ( model, Cmd.none )


{-| Helper method to update the model when the Msg is an wsIncoming.
-}
updateCM : Game -> Model -> Model
updateCM g model =
    let
        q =
            model.gamePlay.quant

        r =
            model.gamePlay.rank
        h = model.gameInfo.hand
    in
        { model
            | wsIncoming = JsonMsg (Ok g)
            , gameInfo =
                { name = withDefault "" <| M.map .name (get (fst g.special) g.players)
                , turn = turn g
                , bidder = bidder g
                , baseStake = g.baseStake
                , multiple = g.multiple
                , bid = g.bid
                , hand = snd g.special
                , prevHand = h
                }
            , players =
                { players = g.players
                , bidder = g.bidder
                , turn = g.turn
                }
            , gamePlay =
                { quant = q
                , rank = r
                , bid = g.bid
                , buttons = withDefault { raiseFlag = False
                                        , chalFlag = False
                                        , countFlag = False
                                        }
                  <| M.map .flags (get (fst g.special) g.players)
                , preResult =
                    (g.bidder == Nothing) && g.inProgress
                }
        }
