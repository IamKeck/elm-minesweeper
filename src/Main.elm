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
    = BeforeStart Difficulty
    | Running
        { board : Mine.Board
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
    = GotMines (List Mine.Position)
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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Running _ ->
            Time.every 300 GotCurrentTime

        _ ->
            Sub.none


init : Flags -> ( Model, Cmd Msg )
init =
    let
        ( x, y ) =
            getBoardSize Easy
    in
    always
        ( BeforeStart Easy
        , Random.generate GotMines <| generateMinePosition x y (getMineNum Easy)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMines mines ->
            case model of
                BeforeStart diff ->
                    let
                        ( sizeX, sizeY ) =
                            getBoardSize diff
                    in
                    ( Running
                        { board = Mine.makeBoard sizeX sizeY mines
                        , flag = 0
                        , startTime = Nothing
                        , currentTime = Nothing
                        , difficulty = diff
                        , selectingDifficulty = diff
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
                BeforeStart diff ->
                    let
                        ( x, y ) =
                            getBoardSize diff
                    in
                    ( BeforeStart diff
                    , Random.generate GotMines <| generateMinePosition x y (getMineNum diff)
                    )

                Running s ->
                    let
                        ( x, y ) =
                            getBoardSize s.selectingDifficulty
                    in
                    ( BeforeStart s.selectingDifficulty
                    , Random.generate GotMines <| generateMinePosition x y (getMineNum s.selectingDifficulty)
                    )

                Win s ->
                    let
                        ( x, y ) =
                            getBoardSize s.selectingDifficulty
                    in
                    ( BeforeStart s.selectingDifficulty
                    , Random.generate GotMines <| generateMinePosition x y (getMineNum s.selectingDifficulty)
                    )

                Lose s ->
                    let
                        ( x, y ) =
                            getBoardSize s.selectingDifficulty
                    in
                    ( BeforeStart s.selectingDifficulty
                    , Random.generate GotMines <| generateMinePosition x y (getMineNum s.selectingDifficulty)
                    )

        GotCurrentTime t ->
            case model of
                Running s ->
                    ( Running { s | currentTime = Just t }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DifficultyChanged diff ->
            case model of
                BeforeStart _ ->
                    ( model, Cmd.none )

                Running s ->
                    ( Running { s | selectingDifficulty = diff }, Cmd.none )

                Win s ->
                    ( Win { s | selectingDifficulty = diff }, Cmd.none )

                Lose s ->
                    ( Lose { s | selectingDifficulty = diff }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        b : Difficulty -> Mine.Board -> Html Msg
        b diff =
            let
                ( x, y ) =
                    getBoardSize diff
            in
            board diff x y
    in
    case model of
        BeforeStart _ ->
            h1 [] [ text "before start" ]

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


board : Difficulty -> Int -> Int -> Mine.Board -> Html Msg
board diff xsize ysize b =
    let
        xs =
            List.range 0 (xsize - 1)

        ys =
            List.range 0 (ysize - 1)

        positions =
            List.concatMap (\y -> List.map (\x -> ( x, y )) xs) ys

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


generateMinePosition : Int -> Int -> Int -> Random.Generator (List Mine.Position)
generateMinePosition xsize ysize mineNum =
    let
        xpoint =
            Random.int 0 (xsize - 1)

        ypoint =
            Random.int 0 (ysize - 1)

        position =
            Random.map2 (\x y -> ( x, y )) xpoint ypoint
    in
    noOverlapRandomList position mineNum


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
