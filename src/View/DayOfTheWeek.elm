module View.DayOfTheWeek exposing (..)

import Array exposing (Array)


longNames : Array String
longNames =
    [ "Sunday"
    , "Monday"
    , "Tuesday"
    , "Wednesday"
    , "Thursday"
    , "Friday"
    , "Saturday"
    ]
        |> Array.fromList


shortNames : Array String
shortNames =
    [ "Sun"
    , "Mon"
    , "Tue"
    , "Wed"
    , "Thu"
    , "Fri"
    , "Sat"
    ]
        |> Array.fromList


toLongString : Int -> String
toLongString n =
    longNames
        |> Array.get (modBy 7 n)
        |> Maybe.withDefault ""


toShortString : Int -> String
toShortString n =
    shortNames
        |> Array.get (modBy 7 n)
        |> Maybe.withDefault ""
