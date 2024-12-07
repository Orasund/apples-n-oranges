module Data.Message exposing (..)

import Array exposing (Array)


next : Int -> String
next n =
    Array.get n ellen
        |> Maybe.withDefault ""


ellen : Array String
ellen =
    [ "Hi, Ellen here! I wanted to check in, if you came on save and sound. Heres a little gift to get you started"
    , "Hi again! I forgot to setup  your telephone. You should now be able to trade with your neighbors!"
    , "Hello, It's me Ellen ;) The local rescue shelter asked if we could take care of a little cat. Would you be interested?"
    ]
        |> Array.fromList
