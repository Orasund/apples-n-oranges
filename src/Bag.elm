module Bag exposing (..)

import Data.Block exposing (Block(..), Optional)
import Dict exposing (Dict)


type alias Bag =
    Dict String ( Optional, Int )


empty : Bag
empty =
    Dict.empty


insert : Optional -> Bag -> Bag
insert =
    add 1


add : Int -> Optional -> Bag -> Bag
add n item =
    Dict.update (Data.Block.toString (OptionalBlock item))
        (\maybe ->
            maybe
                |> Maybe.map
                    (\( _, amount ) ->
                        ( item, amount + n )
                    )
                |> Maybe.withDefault ( item, n )
                |> Just
        )


remove : Optional -> Bag -> Bag
remove item =
    Dict.update (Data.Block.toString (OptionalBlock item))
        (\maybe ->
            maybe
                |> Maybe.andThen
                    (\( _, amount ) ->
                        if amount > 1 then
                            Just ( item, amount - 1 )

                        else
                            Nothing
                    )
        )


get : Optional -> Bag -> Int
get item bag =
    bag
        |> Dict.get (Data.Block.toString (OptionalBlock item))
        |> Maybe.map Tuple.second
        |> Maybe.withDefault 0


toList : Bag -> List ( Optional, Int )
toList bag =
    bag
        |> Dict.toList
        |> List.map Tuple.second


fromList : List ( Optional, Int ) -> Bag
fromList =
    List.foldl (\( a, n ) -> add n a)
        empty


contains : Int -> Optional -> Bag -> Bool
contains n item bag =
    get item bag >= n
