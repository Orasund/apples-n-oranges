module Generator exposing (Builder, generateLevel)

import Dict exposing (Dict)
import Game exposing (Block(..), Fruit(..), Solid(..))
import Level exposing (Level)
import Random exposing (Generator)
import Set exposing (Set)


type alias Random a =
    Generator a


type alias Builder =
    { blocks : Dict ( Int, Int ) Block
    , remainingPositions : Set ( Int, Int )
    , remainingOldSprouts : Set ( Int, Int )
    , columns : Int
    , rows : Int
    }


new : { columns : Int, rows : Int, oldSprouts : Set ( Int, Int ) } -> Builder
new args =
    { blocks = Dict.empty
    , remainingPositions =
        List.range 0 (args.columns - 1)
            |> List.concatMap
                (\x ->
                    List.range 0 (args.rows - 1)
                        |> List.map (Tuple.pair x)
                )
            |> Set.fromList
    , remainingOldSprouts = args.oldSprouts
    , columns = args.columns
    , rows = args.rows
    }


generateLevel :
    { columns : Int
    , rows : Int
    , oldBlocks : Dict ( Int, Int ) Block
    , newSprouts : Int
    , newStone : Int
    , newFruitPairs : Int
    }
    -> Random Level
generateLevel args =
    let
        oldSprouts =
            args.oldBlocks
                |> Dict.filter (\_ block -> block == SolidBlock Sprout)
                |> Dict.keys
                |> Set.fromList
    in
    new
        { columns = args.columns
        , rows = args.rows
        , oldSprouts = oldSprouts
        }
        |> addSolids
            (oldSprouts
                |> Set.toList
                |> List.map (\pos -> ( pos, Sprout ))
            )
        |> addSolids
            (args.oldBlocks
                |> Dict.filter (\_ block -> block == SolidBlock Stone)
                |> Dict.keys
                |> List.map (\pos -> ( pos, Stone ))
            )
        |> Random.constant
        |> andThenRepeat args.newStone (addRandomSolid Stone)
        |> andThenRepeat args.newSprouts addSproutPair
        |> andThenRepeat (Set.size oldSprouts) addFruitPairFromOldSprout
        |> andThenRepeat args.newFruitPairs (addRandomPair FruitBlock)
        |> Random.map build


build : Builder -> Level
build builder =
    { columns = builder.columns
    , rows = builder.rows
    , blocks = builder.blocks |> Dict.toList
    }


randomSolid : Random Solid
randomSolid =
    [ Stone ]
        |> randomFromList
        |> Maybe.withDefault (Random.constant Stone)


generatePairs : Int -> (Fruit -> Block) -> Builder -> Random Builder
generatePairs pairs toBlock builder =
    List.range 0 (pairs - 1)
        |> List.foldl
            (\_ -> Random.andThen (addRandomPair toBlock))
            (builder |> Random.constant)


addSolids : List ( ( Int, Int ), Solid ) -> Builder -> Builder
addSolids solids builder =
    solids
        |> List.foldl
            (\( pos, solid ) ->
                addSolid pos solid
            )
            builder


addRandomSolid : Solid -> Builder -> Random Builder
addRandomSolid solid builder =
    builder.remainingPositions
        |> Set.toList
        |> randomFromList
        |> Maybe.withDefault (Random.constant ( -1, -1 ))
        |> Random.map
            (\pos ->
                addSolid pos solid builder
            )


addSolid : ( Int, Int ) -> Solid -> Builder -> Builder
addSolid pos solid builder =
    { builder
        | remainingPositions = builder.remainingPositions |> Set.remove pos
        , blocks = builder.blocks |> Dict.insert pos (SolidBlock solid)
    }


addFruitPairFromOldSprout : Builder -> Random Builder
addFruitPairFromOldSprout builder =
    case builder.remainingOldSprouts |> Set.toList of
        head :: tail ->
            case findValidPair builder head (Set.fromList tail) of
                Just randomPos ->
                    randomPos
                        |> Random.map
                            (\pos ->
                                { builder | remainingOldSprouts = tail |> Set.fromList |> Set.remove pos }
                                    |> addFruit head Lemon
                                    |> addFruit pos Orange
                            )

                Nothing ->
                    { builder
                        | remainingOldSprouts = Set.fromList tail
                    }
                        |> addSolid head Sprout
                        |> Random.constant

        [] ->
            Random.constant builder


addSproutPair : Builder -> Random Builder
addSproutPair builder =
    randomPair builder
        |> Random.map
            (\list ->
                list |> List.foldl (\pos -> addSolid pos Sprout) builder
            )


randomPair : Builder -> Random (List ( Int, Int ))
randomPair builder =
    Set.toList builder.remainingPositions
        |> randomFromList
        |> Maybe.map
            (Random.andThen
                (\p1 ->
                    builder.remainingPositions
                        |> Set.remove p1
                        |> findValidPair builder p1
                        |> Maybe.map (Random.map (\p2 -> [ p1, p2 ]))
                        |> Maybe.withDefault (Random.constant [ p1 ])
                )
            )
        |> Maybe.withDefault (Random.constant [])


addFruit : ( Int, Int ) -> Fruit -> Builder -> Builder
addFruit pos fruit builder =
    { builder
        | remainingPositions = builder.remainingPositions |> Set.remove pos
        , blocks = builder.blocks |> Dict.insert pos (FruitBlock fruit)
    }


addRandomPair : (Fruit -> Block) -> Builder -> Random Builder
addRandomPair toBlock builder =
    randomPair builder
        |> Random.map
            (\list ->
                case list of
                    p1 :: p2 :: _ ->
                        { builder
                            | remainingPositions =
                                builder.remainingPositions
                                    |> Set.remove p1
                                    |> Set.remove p2
                            , blocks =
                                builder.blocks
                                    |> Dict.insert p1 (toBlock Apple)
                                    |> Dict.insert p2 (toBlock Pear)
                        }

                    [ p1 ] ->
                        { builder | remainingPositions = Set.remove p1 builder.remainingPositions }

                    [] ->
                        builder
            )


findValidPair : Builder -> ( Int, Int ) -> Set ( Int, Int ) -> Maybe (Random ( Int, Int ))
findValidPair builder pos candidates =
    candidates
        |> Set.toList
        |> List.filter (isValidPair builder pos)
        |> randomFromList


isValidPair : Builder -> ( Int, Int ) -> ( Int, Int ) -> Bool
isValidPair builder ( x1, y1 ) ( x2, y2 ) =
    let
        validBlock block =
            case block of
                FruitBlock _ ->
                    True

                SolidBlock _ ->
                    False
    in
    ((x1 == x2)
        && (List.range (min y1 y2 + 1) (max y1 y2 - 1)
                |> List.all
                    (\y ->
                        builder.blocks
                            |> Dict.get ( x1, y )
                            |> Maybe.map validBlock
                            |> Maybe.withDefault True
                    )
           )
    )
        || ((y1 == y2)
                && (List.range (min x1 x2 + 1) (max x1 x2 - 1)
                        |> List.all
                            (\x ->
                                builder.blocks
                                    |> Dict.get ( x, y1 )
                                    |> Maybe.map validBlock
                                    |> Maybe.withDefault True
                            )
                   )
           )


randomFromList : List a -> Maybe (Random a)
randomFromList list =
    case list of
        head :: tail ->
            Random.uniform head tail |> Just

        [] ->
            Nothing


andThenRepeat : Int -> (a -> Random a) -> Random a -> Random a
andThenRepeat n fun rand =
    List.repeat n ()
        |> List.foldl (\() -> Random.andThen fun)
            rand
