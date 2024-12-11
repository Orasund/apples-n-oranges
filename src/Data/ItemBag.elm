module Data.ItemBag exposing (..)

import Data.Block exposing (Block(..), Item)
import Dict exposing (Dict)
import Set exposing (Set)


type alias ItemBag =
    Dict String ( Item, Set ( Int, Int ) )


empty : ItemBag
empty =
    Dict.empty


insert : ( Int, Int ) -> Item -> ItemBag -> ItemBag
insert pos =
    add [ pos ]


add : List ( Int, Int ) -> Item -> ItemBag -> ItemBag
add positions item =
    let
        set =
            Set.fromList positions
    in
    Dict.update (Data.Block.toString (ItemBlock item))
        (\maybe ->
            maybe
                |> Maybe.map
                    (\( _, amount ) ->
                        ( item, Set.union amount set )
                    )
                |> Maybe.withDefault ( item, set )
                |> Just
        )


remove : Item -> ItemBag -> Maybe ( ItemBag, ( Int, Int ) )
remove item bag =
    bag
        |> Dict.get (Data.Block.toString (ItemBlock item))
        |> Maybe.andThen
            (\( _, positions ) ->
                case Set.toList positions of
                    head :: tail ->
                        ( bag
                            |> Dict.insert (Data.Block.toString (ItemBlock item))
                                ( item, Set.fromList tail )
                        , head
                        )
                            |> Just

                    [] ->
                        Nothing
            )


get : Item -> ItemBag -> Int
get item bag =
    bag
        |> Dict.get (Data.Block.toString (ItemBlock item))
        |> Maybe.map Tuple.second
        |> Maybe.map Set.size
        |> Maybe.withDefault 0


toList : ItemBag -> List ( Item, Set ( Int, Int ) )
toList bag =
    bag
        |> Dict.toList
        |> List.map Tuple.second


fromList : List ( Item, List ( Int, Int ) ) -> ItemBag
fromList =
    List.foldl (\( a, n ) -> add n a)
        empty


contains : Int -> Item -> ItemBag -> Bool
contains n item bag =
    get item bag >= n
