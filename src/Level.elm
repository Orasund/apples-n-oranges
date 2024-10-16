module Level exposing (..)

import Game exposing (Fruit)


type alias Level =
    { columns : Int
    , rows : Int
    , fruits : List ( ( Int, Int ), Fruit )
    }


toList : Int -> Level
toList id =
    (case id of
        1 ->
            [ "🍊🍊"
            , "🍎🍎"
            ]

        2 ->
            [ "🍊🍊🍎"
            , "🔹🍎🔹"
            ]

        3 ->
            [ "🍊🍊🍎"
            , "🍎🍎🔹"
            , "🍊🔹🔹"
            ]

        4 ->
            [ "🍊🍎🔹"
            , "🍎🍊🍊"
            , "🍎🔹🔹"
            ]

        5 ->
            [ "🍊🍎🍊"
            , "🍎🍊🍊"
            , "🍎🍎🔹"
            ]

        6 ->
            [ "🍊🍊🍊"
            , "🍊🍎🍎"
            , "🍎🔹🍎"
            ]

        _ ->
            []
    )
        |> fromStrings


fromStrings : List String -> Level
fromStrings list =
    let
        rows =
            List.length list

        columns =
            List.head list |> Maybe.withDefault "" |> String.toList |> List.length

        fruits =
            list
                |> List.indexedMap
                    (\y row ->
                        row
                            |> String.toList
                            |> List.indexedMap
                                (\x string ->
                                    case string of
                                        '🍊' ->
                                            Just ( ( x, y ), Game.Orange )

                                        '🍎' ->
                                            Just ( ( x, y ), Game.Apple )

                                        _ ->
                                            Nothing
                                )
                            |> List.filterMap identity
                    )
                |> List.concat
    in
    { columns = columns, rows = rows, fruits = fruits }
