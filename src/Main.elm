module Main exposing (..)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Mine
import Random
import Task
import Time


type Model
    = BeforeStart
    | Running
        { board : Mine.Board
        , flag : Int
        , startTime : Maybe Time.Posix
        , currentTime : Maybe Time.Posix
        }
    | Win
        { board : Mine.Board
        , startTime : Maybe Time.Posix
        , currentTime : Maybe Time.Posix
        }
    | Lose
        { board : Mine.Board
        , startTime : Maybe Time.Posix
        , currentTime : Maybe Time.Posix
        }


type Msg
    = GotMines (List Mine.Position)
    | GotStartTime Time.Posix
    | GotCurrentTime Time.Posix
    | OpenCell Mine.Position
    | FlagCell Mine.Position
    | QuickOpen Mine.Position
    | ReTry


type alias Flags =
    ()


boardSize : Int
boardSize =
    9


mineNumber : Int
mineNumber =
    10


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Running _ ->
            Time.every 300 GotCurrentTime

        _ ->
            Sub.none


init : Flags -> ( Model, Cmd Msg )
init =
    always
        ( BeforeStart
        , Random.generate GotMines <| generateMinePosition boardSize mineNumber
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMines mines ->
            case model of
                BeforeStart ->
                    ( Running
                        { board = Mine.makeBoard boardSize boardSize mines
                        , flag = 0
                        , startTime = Nothing
                        , currentTime = Nothing
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GotStartTime t ->
            case model of
                Running s ->
                    ( Running { s | startTime = Just t, currentTime = Just t }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        OpenCell position ->
            case model of
                Running state ->
                    let
                        ( newBoard, isOk ) =
                            Mine.openBoard position state.board
                    in
                    if isOk then
                        ( Running { state | board = newBoard }
                        , if state.startTime == Nothing then
                            Task.perform GotStartTime Time.now

                          else
                            Cmd.none
                        )

                    else
                        ( Lose
                            { board = newBoard
                            , startTime = state.startTime
                            , currentTime = state.currentTime
                            }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        FlagCell position ->
            case model of
                Running state ->
                    let
                        ( newBoard, isWin ) =
                            Mine.flagCell position state.board
                    in
                    if isWin then
                        ( Win
                            { board = newBoard
                            , startTime = state.startTime
                            , currentTime = state.currentTime
                            }
                        , Cmd.none
                        )

                    else
                        ( Running
                            { state
                                | board = newBoard
                                , flag = Mine.countFlaggedCell newBoard
                            }
                        , if state.startTime == Nothing then
                            Task.perform GotStartTime Time.now

                          else
                            Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        QuickOpen pos ->
            case model of
                Running state ->
                    let
                        ( newBoard, isOk ) =
                            Mine.quickOpen pos state.board
                    in
                    if isOk then
                        ( Running { state | board = newBoard }, Cmd.none )

                    else
                        ( Lose
                            { board = newBoard
                            , startTime = state.startTime
                            , currentTime = state.currentTime
                            }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        ReTry ->
            ( BeforeStart
            , Random.generate GotMines <| generateMinePosition boardSize mineNumber
            )

        GotCurrentTime t ->
            case model of
                Running s ->
                    ( Running { s | currentTime = Just t }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        b =
            board boardSize
    in
    case model of
        BeforeStart ->
            h1 [] [ text "before start" ]

        Running m ->
            div []
                [ h1 [] [ text "minesweeper" ]
                , p [] [ text <| "flag remaining:" ++ String.fromInt (mineNumber - m.flag) ]
                , p []
                    [ text <|
                        (Maybe.map2 renderTimeDiff m.startTime m.currentTime
                            |> Maybe.map (\s -> s ++ "秒経過")
                            |> Maybe.withDefault ""
                        )
                    ]
                , b m.board
                ]

        Win m ->
            div []
                [ h1 [] [ text "you win" ]
                , p []
                    [ text <|
                        (Maybe.map2 renderTimeDiff m.startTime m.currentTime
                            |> Maybe.map (\s -> s ++ "秒")
                            |> Maybe.withDefault ""
                        )
                    ]
                , div []
                    [ button [ HE.onClick ReTry ] [ text "retry" ]
                    ]
                , b m.board
                ]

        Lose m ->
            div []
                [ h1 [] [ text "you lose" ]
                , div []
                    [ button [ HE.onClick ReTry ] [ text "retry" ]
                    ]
                , b m.board
                ]


board : Int -> Mine.Board -> Html Msg
board size b =
    let
        xys =
            List.range 0 (size - 1)

        positions =
            List.concatMap (\y -> List.map (\x -> ( x, y )) xys) xys

        render pos =
            Dict.get pos b |> Maybe.withDefault (Mine.Mine Mine.MOpen) |> renderCell

        boardSizeS =
            String.fromInt boardSize
    in
    div
        [ HA.style "display" "inline-grid"
        , HA.style "grid-template-columns" <| "repeat(" ++ boardSizeS ++ ", 1fr)"
        ]
        (List.map
            (\p ->
                case Dict.get p b of
                    Nothing ->
                        span [] []

                    Just cell ->
                        div
                            [ HA.class "cell"
                            , HA.classList [ ( "open", Mine.isOpenCell cell ) ]
                            , HE.onClick <| OpenCell p
                            , onRightClick <| FlagCell p
                            , HE.onDoubleClick <| QuickOpen p
                            ]
                            [ render p ]
            )
            positions
        )


renderCell : Mine.CellType -> Html Msg
renderCell t =
    case t of
        Mine.Mine s ->
            case s of
                Mine.MClosed ->
                    span [] [ text "" ]

                Mine.MOpen ->
                    span [] [ text "💣" ]

                Mine.MFlagged ->
                    span [] [ text "🏁" ]

        Mine.Empty s ->
            case s of
                Mine.Opened 0 ->
                    span [] []

                Mine.Opened num ->
                    let
                        nums =
                            String.fromInt num
                    in
                    span [ HA.class <| "openCell-" ++ nums ] [ text nums ]

                Mine.Flagged ->
                    span [] [ text "🏁" ]

                Mine.Closed ->
                    span [] []


renderTimeDiff : Time.Posix -> Time.Posix -> String
renderTimeDiff start end =
    let
        startMil =
            Time.posixToMillis start

        endMil =
            Time.posixToMillis end
    in
    (endMil - startMil) // 1000 |> String.fromInt


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


generateMinePosition : Int -> Int -> Random.Generator (List Mine.Position)
generateMinePosition size mineNum =
    let
        point =
            Random.int 0 (size - 1)

        position =
            Random.map2 (\x y -> ( x, y )) point point
    in
    noOverlapRandomList position mineNum


noOverlapRandomList : Random.Generator a -> Int -> Random.Generator (List a)
noOverlapRandomList gen length =
    let
        lst : List a -> Random.Generator (List a)
        lst list =
            gen
                |> Random.andThen
                    (\newElement ->
                        if List.length list == length then
                            Random.constant list

                        else if List.member newElement list then
                            lst list

                        else
                            newElement :: list |> lst
                    )
    in
    lst []


onRightClick : msg -> Attribute msg
onRightClick m =
    HE.preventDefaultOn "contextmenu" <| JD.succeed ( m, True )
