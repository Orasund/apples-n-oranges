module Bag exposing (..)

import Dict exposing (Dict)


type Item
    = Coin
    | Diamant
    | Worm
    | Snail
    | Stone


type alias Bag =
    Dict String ( Item, Int )


toString : Item -> String
toString item =
    case item of
        Coin ->
            "🪙"

        Worm ->
            "🪱"

        Diamant ->
            "💎"

        Snail ->
            "🐌"

        Stone ->
            "🪨"


empty : Bag
empty =
    Dict.empty


insert : Item -> Bag -> Bag
insert item =
    Dict.update (toString item)
        (\maybe ->
            maybe
                |> Maybe.map
                    (\( _, amount ) ->
                        ( item, amount + 1 )
                    )
                |> Maybe.withDefault ( item, 1 )
                |> Just
        )


toList : Bag -> List ( Item, Int )
toList bag =
    bag
        |> Dict.toList
        |> List.map Tuple.second
