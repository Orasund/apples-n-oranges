module Maths exposing (..)


intersect : ( Int, Int ) -> ( Int, Int ) -> ( Float, Float )
intersect ( x1, y1 ) ( x2, y2 ) =
    ( (toFloat x1 + toFloat x2) / 2
    , (toFloat y1 + toFloat y2) / 2
    )
