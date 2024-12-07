module Bag exposing (..)

import Data.Block exposing (Block)
import Dict exposing (Dict)


type alias Bag =
    Dict String ( Block, Int )


empty : Bag
empty =
    Dict.empty


insert : Block -> Bag -> Bag
insert item =
    Dict.update (Data.Block.toString item)
        (\maybe ->
            maybe
                |> Maybe.map
                    (\( _, amount ) ->
                        ( item, amount + 1 )
                    )
                |> Maybe.withDefault ( item, 1 )
                |> Just
        )


insertAll : List Block -> Bag -> Bag
insertAll list bag =
    List.foldl
        insert
        bag
        list


get : Block -> Bag -> Int
get block bag =
    bag
        |> Dict.get (Data.Block.toString block)
        |> Maybe.map Tuple.second
        |> Maybe.withDefault 0


toList : Bag -> List ( Block, Int )
toList bag =
    bag
        |> Dict.toList
        |> List.map Tuple.second
