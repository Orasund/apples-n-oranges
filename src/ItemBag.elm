module ItemBag exposing (..)

import Data.Block exposing (Block(..), Item)
import Dict exposing (Dict)


type alias ItemBag =
    Dict String ( Item, Int )


empty : ItemBag
empty =
    Dict.empty


insert : Item -> ItemBag -> ItemBag
insert =
    add 1


add : Int -> Item -> ItemBag -> ItemBag
add n item =
    Dict.update (Data.Block.toString (ItemBlock item))
        (\maybe ->
            maybe
                |> Maybe.map
                    (\( _, amount ) ->
                        ( item, amount + n )
                    )
                |> Maybe.withDefault ( item, n )
                |> Just
        )


remove : Item -> ItemBag -> ItemBag
remove item =
    Dict.update (Data.Block.toString (ItemBlock item))
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


get : Item -> ItemBag -> Int
get item bag =
    bag
        |> Dict.get (Data.Block.toString (ItemBlock item))
        |> Maybe.map Tuple.second
        |> Maybe.withDefault 0


toList : ItemBag -> List ( Item, Int )
toList bag =
    bag
        |> Dict.toList
        |> List.map Tuple.second


fromList : List ( Item, Int ) -> ItemBag
fromList =
    List.foldl (\( a, n ) -> add n a)
        empty


contains : Int -> Item -> ItemBag -> Bool
contains n item bag =
    get item bag >= n
