module Maths exposing (..)


intersect : ( Int, Int ) -> ( Int, Int ) -> ( Float, Float )
intersect ( x1, y1 ) ( x2, y2 ) =
    ( (toFloat x1 + toFloat x2) / 2
    , (toFloat y1 + toFloat y2) / 2
    )


length : ( Int, Int ) -> ( Int, Int ) -> Float
length ( x1, y1 ) ( x2, y2 ) =
    let
        x =
            x1 - x2

        y =
            y1 - y2
    in
    sqrt (toFloat (x * x + y * y))


plus : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
plus ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )
