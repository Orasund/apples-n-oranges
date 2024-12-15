module Data.Person exposing (..)


type Job
    = Major
    | Ranger


type alias Person =
    { symbol : String
    , name : String
    , job : Job
    , progress : Int
    }


alice : Person
alice =
    { symbol = "👩🏻"
    , name = "Alice"
    , job = Ranger
    , progress = 0
    }


rick : Person
rick =
    { symbol = "👨🏼"
    , name = "Rick"
    , job = Major
    , progress = 0
    }
