module Mine exposing (..)

import Dict


type CellType
    = Mine MineState
    | Empty EmptyCellState


type MineState
    = MOpen
    | MClosed
    | MFlagged


type EmptyCellState
    = Opened Int
    | Flagged
    | Closed


type alias Position =
    ( Int, Int )


type alias Board =
    Dict.Dict Position CellType


type alias OkToContinue =
    Bool


type alias IsWin =
    Bool


makeFakeBoard : Int -> Int -> Board
makeFakeBoard xSize ySize =
    let
        xList =
            List.range 0 (xSize - 1)

        yList =
            List.range 0 (ySize - 1)

        positionList =
            listMap2 (\x y -> ( x, y )) xList yList

        emptyCell =
            Empty Closed
    in
    List.foldl (\position board -> Dict.insert position emptyCell board) Dict.empty positionList


makeBoard : Int -> Int -> List Position -> Board
makeBoard xSize ySize mines =
    let
        xList =
            List.range 0 (xSize - 1)

        yList =
            List.range 0 (ySize - 1)

        positionList =
            listMap2 (\x y -> ( x, y )) xList yList

        emptyCell =
            Empty Closed

        allEmptyList =
            List.foldl (\position board -> Dict.insert position emptyCell board) Dict.empty positionList
    in
    List.foldl (\position board -> Dict.insert position (Mine MClosed) board) allEmptyList mines


openBoard : Position -> Board -> ( Board, OkToContinue )
openBoard position board =
    case Dict.get position board of
        Just cell ->
            case cell of
                Mine _ ->
                    ( Dict.insert position (Mine MOpen) board, False )

                Empty state ->
                    case state of
                        Opened _ ->
                            ( board, True )

                        _ ->
                            let
                                mineNum =
                                    getNumberOfMineAroundCell position board
                            in
                            case mineNum of
                                0 ->
                                    Dict.insert position (Empty (Opened 0)) board
                                        |> openManyCell (getPositionsAroundCell position)

                                _ ->
                                    ( Dict.insert position (Empty (Opened mineNum)) board, True )

        Nothing ->
            ( board, True )


openManyCell : List Position -> Board -> ( Board, OkToContinue )
openManyCell positions board =
    List.foldl
        (\pos ( boardI, ok ) ->
            case ok of
                True ->
                    openBoard pos boardI

                False ->
                    ( boardI, False )
        )
        ( board, True )
        positions


quickOpen : Position -> Board -> ( Board, OkToContinue )
quickOpen pos board =
    case Dict.get pos board of
        Nothing ->
            ( board, True )

        Just cell ->
            case cell of
                Mine _ ->
                    ( board, True )

                Empty state ->
                    case state of
                        Opened num ->
                            let
                                aroundPos =
                                    getPositionsAroundCell pos

                                aroundCell =
                                    List.map (\p -> Dict.get p board) aroundPos |> listFromMaybe

                                notFlaggedCellPos =
                                    List.filter
                                        (\position ->
                                            case Dict.get position board of
                                                Nothing ->
                                                    False

                                                Just c ->
                                                    isFlaggedCell c |> not
                                        )
                                        aroundPos

                                flaggedCellNum =
                                    List.filter (\c -> isFlaggedCell c) aroundCell |> List.length
                            in
                            if flaggedCellNum == num then
                                openManyCell notFlaggedCellPos board

                            else
                                ( board, True )

                        _ ->
                            ( board, True )


flagCell : Position -> Board -> ( Board, IsWin )
flagCell pos board =
    let
        mineCell =
            countMineCell board

        flaggedCell =
            countFlaggedCell board
    in
    if flaggedCell >= mineCell then
        ( board, False )

    else
        case Dict.get pos board of
            Nothing ->
                ( board, False )

            Just c ->
                case c of
                    Mine state ->
                        case state of
                            MFlagged ->
                                ( Dict.insert pos (Mine MClosed) board, False )

                            _ ->
                                let
                                    newBoard =
                                        Dict.insert pos (Mine MFlagged) board
                                in
                                ( newBoard, isWin newBoard )

                    Empty state ->
                        case state of
                            Closed ->
                                let
                                    newBoard =
                                        Dict.insert pos (Empty Flagged) board
                                in
                                ( newBoard, False )

                            Flagged ->
                                let
                                    newBoard =
                                        Dict.insert pos (Empty Closed) board
                                in
                                ( newBoard, False )

                            _ ->
                                ( board, False )


isFlaggedCell : CellType -> Bool
isFlaggedCell c =
    case c of
        Mine state ->
            case state of
                MFlagged ->
                    True

                _ ->
                    False

        Empty state ->
            case state of
                Flagged ->
                    True

                _ ->
                    False


countFlaggedCell : Board -> Int
countFlaggedCell board =
    Dict.values board |> List.filter isFlaggedCell |> List.length


isMineCell : CellType -> Bool
isMineCell cell =
    case cell of
        Mine _ ->
            True

        Empty _ ->
            False


countMineCell : Board -> Int
countMineCell board =
    Dict.values board |> List.filter isMineCell |> List.length


isWin : Board -> Bool
isWin board =
    let
        cells =
            Dict.values board

        flaggedCells =
            List.filter isFlaggedCell cells

        mineCells =
            List.filter isMineCell cells
    in
    flaggedCells == mineCells


isOpenCell : CellType -> Bool
isOpenCell cell =
    case cell of
        Mine state ->
            case state of
                MOpen ->
                    True

                _ ->
                    False

        Empty state ->
            case state of
                Opened _ ->
                    True

                _ ->
                    False


getPositionsAroundCell : Position -> List Position
getPositionsAroundCell ( x, y ) =
    [ ( x - 1, y )
    , ( x - 1, y - 1 )
    , ( x - 1, y + 1 )
    , ( x, y - 1 )
    , ( x, y + 1 )
    , ( x + 1, y - 1 )
    , ( x + 1, y )
    , ( x + 1, y + 1 )
    ]


getNumberOfMineAroundCell : Position -> Board -> Int
getNumberOfMineAroundCell position board =
    position
        :: getPositionsAroundCell position
        |> List.map (\pos -> Dict.get pos board)
        |> listFromMaybe
        |> List.filter
            (\cell ->
                case cell of
                    Mine _ ->
                        True

                    _ ->
                        False
            )
        |> List.length


listFromMaybe : List (Maybe a) -> List a
listFromMaybe list =
    List.foldr
        (\elem lst ->
            case elem of
                Nothing ->
                    lst

                Just a ->
                    a :: lst
        )
        []
        list


listMap2 : (a -> b -> c) -> List a -> List b -> List c
listMap2 f l1 l2 =
    List.concatMap (\a -> List.map (\b -> f a b) l2) l1
