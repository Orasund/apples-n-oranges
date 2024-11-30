module Bag exposing (..)

import Block exposing (Block)
import Dict exposing (Dict)


type Item
    = Coin
    | Diamant
    | Worm
    | Snail
    | Stone


type alias Bag =
    Dict String ( Block, Int )


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


insert : Block -> Bag -> Bag
insert item =
    Dict.update (Block.toString item)
        (\maybe ->
            maybe
                |> Maybe.map
                    (\( _, amount ) ->
                        ( item, amount + 1 )
                    )
                |> Maybe.withDefault ( item, 1 )
                |> Just
        )


toList : Bag -> List ( Block, Int )
toList bag =
    bag
        |> Dict.toList
        |> List.map Tuple.second
