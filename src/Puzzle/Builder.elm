module Puzzle.Builder exposing (generateLevel)

import Dict exposing (Dict)
import Level exposing (Block(..), Fruit(..), Optional(..), Puzzle, Solid(..), isValidPair)
import Random exposing (Generator)
import Set exposing (Set)


type alias Random a =
    Generator a


type alias Builder =
    { blocks : Dict ( Int, Int ) Block
    , remainingPositions : Set ( Int, Int )

    --, remainingOldSprouts : Set ( Int, Int )
    , columns : Int
    , rows : Int
    }


new :
    { columns : Int
    , rows : Int

    --, oldSprouts : Set ( Int, Int )
    }
    -> Builder
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

    --, remainingOldSprouts = args.oldSprouts
    , columns = args.columns
    , rows = args.rows
    }


generateLevel :
    { columns : Int
    , rows : Int
    , oldBlocks : Dict ( Int, Int ) Block
    , newStone : Int
    , newDynamite : Int
    , newFruitPairs : Int
    , newLemonPairs : Int
    , newGrapePairs : Int
    , fishAndRod : Int
    }
    -> Random Puzzle
generateLevel args =
    {--let
        oldSprouts =
            args.oldBlocks
                |> Dict.filter (\_ block -> block == SolidBlock Sprout)
                |> Dict.keys
                |> Set.fromList
    in--}
    new
        { columns = args.columns
        , rows = args.rows

        --, oldSprouts = oldSprouts
        }
        |> addBlocks
            (args.oldBlocks
                |> Dict.filter
                    (\_ block ->
                        case block of
                            OptionalBlock _ ->
                                False

                            _ ->
                                True
                    )
                |> Dict.toList
            )
        |> Random.constant
        |> andThenRepeat args.newStone (addRandomSolid Stone)
        |> andThenRepeat args.newDynamite (randomAddOptionalNear (SolidBlock Stone) Dynamite)
        |> andThenRepeat (args.fishAndRod // 2) (addRandomPair FishingRod (OptionalBlock Fish))
        |> andThenRepeat args.newLemonPairs
            (\b ->
                Random.uniform Apple [ Orange ]
                    |> Random.andThen
                        (\fruit ->
                            addRandomPair (FruitBlock fruit) (FruitBlock Lemon) b
                        )
            )
        |> andThenRepeat args.newGrapePairs
            (\b ->
                Random.uniform Apple [ Orange, Lemon ]
                    |> Random.andThen
                        (\fruit ->
                            addRandomPair (FruitBlock fruit) (FruitBlock Grapes) b
                        )
            )
        |> andThenRepeat args.newFruitPairs (addRandomPair (FruitBlock Apple) (FruitBlock Orange))
        |> andThenRepeat (args.fishAndRod // 2) (addRandomPair FishingRod (OptionalBlock Fish))
        |> Random.map build


build : Builder -> Puzzle
build builder =
    { columns = builder.columns
    , rows = builder.rows
    , blocks = builder.blocks |> Dict.toList
    }


addBlocks : List ( ( Int, Int ), Block ) -> Builder -> Builder
addBlocks solids builder =
    solids
        |> List.foldl
            (\( pos, solid ) ->
                addBlock pos solid
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
                addBlock pos (SolidBlock solid) builder
            )


randomAddOptionalNear : Block -> Optional -> Builder -> Random Builder
randomAddOptionalNear block optional builder =
    builder.blocks
        |> Dict.filter (\_ b -> b == block)
        |> Dict.keys
        |> randomFromList
        |> Maybe.map
            (Random.andThen
                (\p1 ->
                    builder.remainingPositions
                        |> findValidPair
                            (isValidPair builder
                                [ block ]
                                p1
                            )
                        |> Maybe.map (Random.map (\p2 -> addBlock p2 (OptionalBlock optional) builder))
                        |> Maybe.withDefault (Random.constant builder)
                )
            )
        |> Maybe.withDefault (Random.constant builder)


addBlock : ( Int, Int ) -> Block -> Builder -> Builder
addBlock pos block builder =
    { builder
        | remainingPositions = builder.remainingPositions |> Set.remove pos
        , blocks = builder.blocks |> Dict.insert pos block
    }


randomPair : List Block -> Builder -> Random (List ( Int, Int ))
randomPair validBlocks builder =
    Set.toList builder.remainingPositions
        |> randomFromList
        |> Maybe.map
            (Random.andThen
                (\p1 ->
                    builder.remainingPositions
                        |> Set.remove p1
                        |> findValidPair
                            (isValidPair builder validBlocks p1)
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


addRandomPair : Block -> Block -> Builder -> Random Builder
addRandomPair b1 b2 builder =
    randomPair [ b1, b2 ] builder
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
                                    |> Dict.insert p1 b1
                                    |> Dict.insert p2 b2
                        }

                    [ p1 ] ->
                        { builder | remainingPositions = Set.remove p1 builder.remainingPositions }

                    [] ->
                        builder
            )


findValidPair : (( Int, Int ) -> Bool) -> Set ( Int, Int ) -> Maybe (Random ( Int, Int ))
findValidPair fun candidates =
    candidates
        |> Set.toList
        |> List.filter fun
        |> randomFromList


isValidPair : Builder -> List Block -> ( Int, Int ) -> ( Int, Int ) -> Bool
isValidPair builder validBlocks ( x1, y1 ) ( x2, y2 ) =
    let
        validBlock block =
            List.member block validBlocks
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
