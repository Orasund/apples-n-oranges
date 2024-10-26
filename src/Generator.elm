module Generator exposing (..)

import Dict exposing (Dict)
import Game exposing (Block(..), Fruit(..), Solid(..))
import Level exposing (Level)
import Random exposing (Generator)
import Set exposing (Set)


type alias Random a =
    Generator a


type alias Builder =
    { fruits : Dict ( Int, Int ) Block
    , remaining : Set ( Int, Int )
    , columns : Int
    , rows : Int
    }


new : { columns : Int, rows : Int } -> Builder
new args =
    { fruits = Dict.empty
    , remaining =
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


build : Builder -> Level
build builder =
    { columns = builder.columns
    , rows = builder.rows
    , fruits = builder.fruits |> Dict.toList
    }


randomSolid : Random Solid
randomSolid =
    [ Stone ]
        |> randomFromList
        |> Maybe.withDefault (Random.constant Stone)


generate : { pairs : Int, columns : Int, rows : Int, solids : Int } -> Random Level
generate args =
    List.range 0 (args.pairs - 1)
        |> List.foldl (\_ -> Random.andThen addPair)
            (Random.list args.solids randomSolid
                |> Random.andThen
                    (List.foldl
                        (\solid ->
                            Random.andThen (addSolid solid)
                        )
                        (new { columns = args.columns, rows = args.rows } |> Random.constant)
                    )
            )
        |> Random.map build


addSolid : Solid -> Builder -> Random Builder
addSolid solid builder =
    Set.toList builder.remaining
        |> randomFromList
        |> Maybe.map
            (Random.map
                (\p1 ->
                    { builder
                        | remaining = builder.remaining |> Set.remove p1
                        , fruits = builder.fruits |> Dict.insert p1 (SolidBlock solid)
                    }
                )
            )
        |> Maybe.withDefault (Random.constant builder)


addPair : Builder -> Random Builder
addPair builder =
    Set.toList builder.remaining
        |> randomFromList
        |> Maybe.map
            (Random.andThen
                (\p1 ->
                    builder.remaining
                        |> Set.remove p1
                        |> Set.toList
                        |> List.filter (isValidPair builder p1)
                        |> randomFromList
                        |> Maybe.map
                            (Random.map
                                (\p2 ->
                                    { builder
                                        | remaining =
                                            builder.remaining
                                                |> Set.remove p1
                                                |> Set.remove p2
                                        , fruits =
                                            builder.fruits
                                                |> Dict.insert p1 (FruitBlock Apple)
                                                |> Dict.insert p2 (FruitBlock Orange)
                                    }
                                )
                            )
                        |> Maybe.withDefault
                            ({ builder | remaining = Set.remove p1 builder.remaining }
                                |> Random.constant
                            )
                )
            )
        |> Maybe.withDefault (Random.constant builder)


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
        && (List.range (min y1 y2) (max y1 y2)
                |> List.all
                    (\y ->
                        builder.fruits
                            |> Dict.get ( x1, y )
                            |> Maybe.map validBlock
                            |> Maybe.withDefault True
                    )
           )
    )
        || ((y1 == y2)
                && (List.range (min x1 x2) (max x1 x2)
                        |> List.all
                            (\x ->
                                builder.fruits
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
