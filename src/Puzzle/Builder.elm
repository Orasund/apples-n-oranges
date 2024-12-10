module Puzzle.Builder exposing (Group(..), generate)

import Data.Block exposing (Block(..), Item(..), Organic(..))
import Dict exposing (Dict)
import Level exposing (Puzzle, isValidPair)
import Random exposing (Generator)
import Set exposing (Set)


type alias Random a =
    Generator a


type Group
    = Pair Block Block
    | SingleBlock Block


type alias Builder =
    { blocks : Dict ( Int, Int ) Block
    , remainingPositions : Set ( Int, Int )
    , solids : Set ( Int, Int )
    , columns : Int
    , rows : Int
    }


new :
    { columns : Int
    , rows : Int
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
    , solids = Set.empty
    , columns = args.columns
    , rows = args.rows
    }


generate :
    { pairs : List Group
    , solids : Int
    }
    -> Random Puzzle
generate args =
    let
        initial =
            new
                { columns = 6
                , rows = 6
                }
                |> addSolids args.solids
    in
    args.pairs
        |> List.foldl
            (\group ->
                case group of
                    Pair block1 block2 ->
                        Random.andThen (addRandomPair block1 block2)

                    SingleBlock block ->
                        Random.andThen (addRandomBlock block)
            )
            initial
        |> Random.map build


addSolids : Int -> Builder -> Random Builder
addSolids n builder =
    builder.remainingPositions
        |> Set.toList
        |> shuffle
        |> Random.map
            (\list ->
                { builder
                    | remainingPositions = List.drop n list |> Set.fromList
                    , solids = List.take n list |> Set.fromList
                }
            )


build : Builder -> Puzzle
build builder =
    { columns = builder.columns
    , rows = builder.rows
    , blocks = builder.blocks |> Dict.toList
    , solids = builder.solids
    }


addRandomBlock : Block -> Builder -> Random Builder
addRandomBlock block builder =
    builder.remainingPositions
        |> Set.toList
        |> randomFromList
        |> Maybe.map
            (Random.map
                (\pos ->
                    addBlock pos block builder
                )
            )
        |> Maybe.withDefault (Random.constant builder)


addBlock : ( Int, Int ) -> Block -> Builder -> Builder
addBlock pos block builder =
    { builder
        | remainingPositions = builder.remainingPositions |> Set.remove pos
        , blocks = builder.blocks |> Dict.insert pos block
    }


randomPair : Builder -> Random (List ( Int, Int ))
randomPair builder =
    Set.toList builder.remainingPositions
        |> randomFromList
        |> Maybe.map
            (Random.andThen
                (\p1 ->
                    builder.remainingPositions
                        |> Set.remove p1
                        |> findValidPair
                            (isValidPair builder p1)
                        |> Maybe.map (Random.map (\p2 -> [ p1, p2 ]))
                        |> Maybe.withDefault (Random.constant [ p1 ])
                )
            )
        |> Maybe.withDefault (Random.constant [])


addRandomPair : Block -> Block -> Builder -> Random Builder
addRandomPair b1 b2 builder =
    randomPair builder
        |> Random.map
            (\list ->
                case list of
                    p1 :: p2 :: _ ->
                        builder
                            |> addBlock p1 b1
                            |> addBlock p2 b2

                    _ ->
                        builder
            )


findValidPair : (( Int, Int ) -> Bool) -> Set ( Int, Int ) -> Maybe (Random ( Int, Int ))
findValidPair fun candidates =
    candidates
        |> Set.toList
        |> List.filter fun
        |> randomFromList


isValidPair : Builder -> ( Int, Int ) -> ( Int, Int ) -> Bool
isValidPair builder ( x1, y1 ) ( x2, y2 ) =
    let
        isNotSolid pos =
            Dict.member pos builder.blocks
                || Set.member pos builder.solids
    in
    ((x1 == x2)
        && (List.range (min y1 y2 + 1) (max y1 y2 - 1)
                |> List.all (\y -> isNotSolid ( x1, y ) |> not)
           )
    )
        || ((y1 == y2)
                && (List.range (min x1 x2 + 1) (max x1 x2 - 1)
                        |> List.all
                            (\x -> isNotSolid ( x, y1 ) |> not)
                   )
           )


randomFromList : List a -> Maybe (Random a)
randomFromList list =
    case list of
        head :: tail ->
            Random.uniform head tail |> Just

        [] ->
            Nothing


shuffle : List a -> Random (List a)
shuffle list =
    Random.list (List.length list) (Random.float 0 1)
        |> Random.map
            (\randomList ->
                List.map2 Tuple.pair
                    list
                    randomList
                    |> List.sortBy Tuple.second
                    |> List.map Tuple.first
            )
