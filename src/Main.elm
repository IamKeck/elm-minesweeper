module Main exposing (..)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Mine
import Random
import Set
import Task
import Time


type Model
    = Running
        { board : Mine.Board
        , hasRealBoard : Bool
        , flag : Int
        , startTime : Maybe Time.Posix
        , currentTime : Maybe Time.Posix
        , difficulty : Difficulty
        , selectingDifficulty : Difficulty
        }
    | Win
        { board : Mine.Board
        , startTime : Maybe Time.Posix
        , currentTime : Maybe Time.Posix
        , difficulty : Difficulty
        , selectingDifficulty : Difficulty
        }
    | Lose
        { board : Mine.Board
        , startTime : Maybe Time.Posix
        , currentTime : Maybe Time.Posix
        , difficulty : Difficulty
        , selectingDifficulty : Difficulty
        }


type Msg
    = GotBoard Mine.Board
    | GotStartTime Time.Posix
    | GotCurrentTime Time.Posix
    | OpenCell Mine.Position
    | FlagCell Mine.Position
    | QuickOpen Mine.Position
    | DifficultyChanged Difficulty
    | ReTry


type Difficulty
    = Easy
    | Normal
    | Hard


type alias Flags =
    ()


type CellRenderMode
    = RunningMode
    | FailMode


makeInitialState : Difficulty -> Model
makeInitialState diff =
    let
        ( x, y ) =
            getBoardSize diff
    in
    Running
        { board = Mine.makeFakeBoard x y
        , hasRealBoard = False
        , flag = getMineNum diff
        , startTime = Nothing
        , currentTime = Nothing
        , difficulty = diff
        , selectingDifficulty = diff
        }


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
        ( makeInitialState Easy
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBoard b ->
            case model of
                Running state ->
                    ( Running
                        { state | board = b, hasRealBoard = True }
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
                    if state.hasRealBoard == False then
                        ( model
                        , Cmd.batch
                            [ generateBoard position state.difficulty
                                |> Random.generate GotBoard
                            , Task.perform GotStartTime Time.now
                            ]
                        )

                    else
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
                                , difficulty = state.difficulty
                                , selectingDifficulty = state.selectingDifficulty
                                }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        FlagCell position ->
            case model of
                Running state ->
                    if not state.hasRealBoard then
                        ( model, Cmd.none )

                    else
                        let
                            ( newBoard, isWin ) =
                                Mine.flagCell position state.board
                        in
                        if isWin then
                            ( Win
                                { board = newBoard
                                , startTime = state.startTime
                                , currentTime = state.currentTime
                                , difficulty = state.difficulty
                                , selectingDifficulty = state.difficulty
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
                            , difficulty = state.difficulty
                            , selectingDifficulty = state.difficulty
                            }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        ReTry ->
            case model of
                Running s ->
                    ( makeInitialState s.selectingDifficulty
                    , Cmd.none
                    )

                Win s ->
                    ( makeInitialState s.selectingDifficulty
                    , Cmd.none
                    )

                Lose s ->
                    ( makeInitialState s.selectingDifficulty
                    , Cmd.none
                    )

        GotCurrentTime t ->
            case model of
                Running s ->
                    ( Running { s | currentTime = Just t }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DifficultyChanged diff ->
            case model of
                Running s ->
                    ( Running { s | selectingDifficulty = diff }, Cmd.none )

                Win s ->
                    ( Win { s | selectingDifficulty = diff }, Cmd.none )

                Lose s ->
                    ( Lose { s | selectingDifficulty = diff }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        renderMode =
            case model of
                Running _ ->
                    RunningMode

                Win _ ->
                    RunningMode

                Lose _ ->
                    FailMode

        b : Difficulty -> Mine.Board -> Html Msg
        b diff =
            let
                ( x, y ) =
                    getBoardSize diff
            in
            board renderMode diff x y
    in
    case model of
        Running m ->
            let
                mineNumber =
                    getMineNum m.difficulty
            in
            div []
                [ h1 [] [ text "mine sweeper" ]
                , p [] [ text <| "🏁 × " ++ String.fromInt (mineNumber - m.flag) ]
                , p []
                    [ text <|
                        (Maybe.map2 renderTimeDiff m.startTime m.currentTime
                            |> Maybe.map (\s -> s ++ " sec.")
                            |> Maybe.withDefault ""
                        )
                    ]
                , div []
                    [ difficultySelector m.selectingDifficulty
                    , button [ HE.onClick ReTry ] [ text "retry" ]
                    ]
                , b m.difficulty m.board
                ]

        Win m ->
            div []
                [ h1 [] [ text "you win" ]
                , div []
                    [ difficultySelector m.selectingDifficulty
                    , button [ HE.onClick ReTry ] [ text "retry" ]
                    ]
                , p []
                    [ text <|
                        (Maybe.map2 renderTimeDiff m.startTime m.currentTime
                            |> Maybe.map (\s -> s ++ " sec.")
                            |> Maybe.withDefault ""
                        )
                    ]
                , b m.difficulty m.board
                ]

        Lose m ->
            div []
                [ h1 [] [ text "you lose" ]
                , div []
                    [ difficultySelector m.selectingDifficulty
                    , button [ HE.onClick ReTry ] [ text "retry" ]
                    ]
                , b m.difficulty m.board
                ]


board : CellRenderMode -> Difficulty -> Int -> Int -> Mine.Board -> Html Msg
board renderMode diff xsize ysize b =
    let
        xs =
            List.range 0 (xsize - 1)

        ys =
            List.range 0 (ysize - 1)

        positions =
            List.concatMap (\y -> List.map (\x -> ( x, y )) xs) ys

        renderCell =
            case renderMode of
                RunningMode ->
                    renderCellRunning

                FailMode ->
                    renderCellFailed

        render pos =
            Dict.get pos b |> Maybe.withDefault (Mine.Mine Mine.MOpen) |> renderCell

        ( boardX, _ ) =
            getBoardSize diff

        boardSizeS =
            String.fromInt boardX
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


renderCellRunning : Mine.CellType -> Html Msg
renderCellRunning t =
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


renderCellFailed : Mine.CellType -> Html Msg
renderCellFailed t =
    case t of
        Mine.Mine s ->
            case s of
                Mine.MClosed ->
                    span [] [ text "💣" ]

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
                    div
                        [ HA.style "position" "relative"
                        , HA.style "height" "100%"
                        ]
                        [ span
                            [ HA.style "line-height" "2em"
                            ]
                            [ text "🏁" ]
                        , span
                            [ HA.style "position" "absolute"
                            , HA.style "font-size" "2em"
                            , HA.style "bottom" "0"
                            , HA.style "left" "0"
                            , HA.style "color" "red"
                            ]
                            [ text "☓" ]
                        ]

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


generateBoard : Mine.Position -> Difficulty -> Random.Generator Mine.Board
generateBoard firstPosition diff =
    let
        ( xsize, ysize ) =
            getBoardSize diff

        mineNum =
            getMineNum diff

        safePos =
            Mine.getPositionsAroundCell firstPosition
                |> (\area -> firstPosition :: area)
                |> Set.fromList

        xpoint =
            Random.int 0 (xsize - 1)

        ypoint =
            Random.int 0 (ysize - 1)

        position : () -> Random.Generator Mine.Position
        position _ =
            Random.map2 (\x y -> ( x, y )) xpoint ypoint
                |> Random.andThen
                    (\newPos ->
                        if Set.member newPos safePos then
                            position ()

                        else
                            Random.constant newPos
                    )
    in
    noOverlapRandomList (position ()) mineNum
        |> Random.map
            (\mineList ->
                Mine.makeBoard xsize ysize mineList
                    |> Mine.openBoard firstPosition
                    |> Tuple.first
            )


getBoardSize : Difficulty -> ( Int, Int )
getBoardSize diff =
    case diff of
        Easy ->
            ( 9, 9 )

        Normal ->
            ( 16, 16 )

        Hard ->
            ( 30, 16 )


getMineNum : Difficulty -> Int
getMineNum diff =
    case diff of
        Easy ->
            10

        Normal ->
            40

        Hard ->
            99


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


difficultySelector : Difficulty -> Html Msg
difficultySelector diff =
    let
        handler : String -> Msg
        handler s =
            case s of
                "easy" ->
                    DifficultyChanged Easy

                "normal" ->
                    DifficultyChanged Normal

                "hard" ->
                    DifficultyChanged Hard

                _ ->
                    DifficultyChanged Easy

        onChange : Attribute Msg
        onChange =
            HE.on "change" <| JD.map handler HE.targetValue
    in
    select [ onChange ]
        [ option [ HA.value "easy", HA.selected <| diff == Easy ] [ text "easy" ]
        , option [ HA.value "normal", HA.selected <| diff == Normal ] [ text "normal" ]
        , option [ HA.value "hard", HA.selected <| diff == Hard ] [ text "hard" ]
        ]
