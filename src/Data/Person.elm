module Data.Person exposing (..)


type Job
    = Major
    | Ranger


type alias Person =
    { symbol : String
    , name : String
    , job : Job
    }


alice : Person
alice =
    { symbol = "👩🏻"
    , name = "Alice"
    , job = Ranger
    }


rick : Person
rick =
    { symbol = "👨🏼"
    , name = "Rick"
    , job = Major
    }


jobToString : Job -> String
jobToString job =
    case job of
        Ranger ->
            "Ranger"

        Major ->
            "Major"
