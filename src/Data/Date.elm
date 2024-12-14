module Data.Date exposing (..)


type alias Date =
    ( Int, Int, Int )


daysInAMonth =
    28


zero : Date
zero =
    ( 0, 0, 0 )


day : Date -> Int
day ( _, _, i ) =
    i


summer : Date -> Bool
summer ( _, i, _ ) =
    i == 0


year : Date -> Int
year ( i, _, _ ) =
    i


next : Date -> Date
next ( y, s, d ) =
    if d == daysInAMonth then
        if s == 0 then
            ( y, 1, 1 )

        else
            ( y + 1, 0, 1 )

    else
        ( y, s, d + 1 )


listOfDaysInMonth : Date -> List Date
listOfDaysInMonth ( y, s, _ ) =
    List.range 1 daysInAMonth
        |> List.map (\d -> ( y, s, d ))
