module Generator exposing (..)

import Dict exposing (Dict)
import Game exposing (Fruit(..))
import Level exposing (Level)
import Random exposing (Generator)
import Set exposing (Set)


type alias Random a =
    Generator a


type alias Builder =
    { fruits : Dict ( Int, Int ) Fruit
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


generate : { pairs : Int, columns : Int, rows : Int } -> Random Level
generate args =
    List.range 0 (args.pairs - 1)
        |> List.foldl (\_ -> Random.andThen addPair)
            (new { columns = args.columns, rows = args.rows } |> Random.constant)
        |> Random.map build


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
                        |> List.filter
                            (\( x2, y2 ) ->
                                let
                                    ( x1, y1 ) =
                                        p1
                                in
                                x1 == x2 || y1 == y2
                            )
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
                                                |> Dict.insert p1 Apple
                                                |> Dict.insert p2 Orange
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


randomFromList : List a -> Maybe (Random a)
randomFromList list =
    case list of
        head :: tail ->
            Random.uniform head tail |> Just

        [] ->
            Nothing
