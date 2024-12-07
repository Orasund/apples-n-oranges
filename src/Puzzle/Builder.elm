module Puzzle.Builder exposing (Group(..), generateFromGroup)

import Bag exposing (Bag)
import Data.Block exposing (Block(..), Optional(..), Organic(..))
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
    , bag : Bag
    , columns : Int
    , rows : Int
    }


maxAmount : number
maxAmount =
    8


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
    , bag = Bag.empty
    , columns = args.columns
    , rows = args.rows
    }


generateFromGroup :
    Dict ( Int, Int ) Block
    -> List Group
    -> Random Puzzle
generateFromGroup oldBlocks groups =
    let
        initial =
            new
                { columns = 6
                , rows = 6
                }
                |> addBlocks
                    (oldBlocks
                        |> Dict.filter (\_ -> Data.Block.isPersistant)
                        |> Dict.toList
                    )
                |> Random.constant
    in
    groups
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


addRandomBlock : Block -> Builder -> Random Builder
addRandomBlock block builder =
    if Bag.get block builder.bag < maxAmount then
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

    else
        Random.constant builder


addBlock : ( Int, Int ) -> Block -> Builder -> Builder
addBlock pos block builder =
    { builder
        | remainingPositions = builder.remainingPositions |> Set.remove pos
        , blocks = builder.blocks |> Dict.insert pos block
        , bag = builder.bag |> Bag.insert block
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
                        if Bag.get b1 builder.bag < maxAmount && Bag.get b2 builder.bag < maxAmount then
                            builder
                                |> addBlock p1 b1
                                |> addBlock p2 b2

                        else
                            builder

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
    ((x1 == x2)
        && (List.range (min y1 y2 + 1) (max y1 y2 - 1)
                |> List.all (\y -> Dict.get ( x1, y ) builder.blocks == Nothing)
           )
    )
        || ((y1 == y2)
                && (List.range (min x1 x2 + 1) (max x1 x2 - 1)
                        |> List.all
                            (\x -> Dict.get ( x, y1 ) builder.blocks == Nothing)
                   )
           )


randomFromList : List a -> Maybe (Random a)
randomFromList list =
    case list of
        head :: tail ->
            Random.uniform head tail |> Just

        [] ->
            Nothing
