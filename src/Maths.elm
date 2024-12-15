module Maths exposing (..)

import Random exposing (Generator)


type alias Random a =
    Generator a


intersect : ( Int, Int ) -> ( Int, Int ) -> ( Float, Float )
intersect ( x1, y1 ) ( x2, y2 ) =
    ( (toFloat x1 + toFloat x2) / 2
    , (toFloat y1 + toFloat y2) / 2
    )


length : ( Int, Int ) -> Float
length ( x, y ) =
    sqrt (toFloat (x * x + y * y))


distance : ( Int, Int ) -> ( Int, Int ) -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    length ( x1 - x2, y1 - y2 )


plus : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
plus ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


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
