module Puzzle.Builder exposing (Group(..), generateFromGroup)

import Block exposing (Block(..), Fruit(..), Optional(..))
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
    , columns = args.columns
    , rows = args.rows
    }


generateFromGroup :
    { columns : Int
    , rows : Int
    , oldBlocks : Dict ( Int, Int ) Block
    }
    -> List Group
    -> Random Puzzle
generateFromGroup args groups =
    let
        initial =
            new
                { columns = args.columns
                , rows = args.rows
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
    builder.remainingPositions
        |> Set.toList
        |> randomFromList
        |> Maybe.withDefault (Random.constant ( -1, -1 ))
        |> Random.map
            (\pos ->
                addBlock pos block builder
            )


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
