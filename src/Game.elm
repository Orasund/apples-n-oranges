module Game exposing (..)

import Dict exposing (Dict)


type alias FruitId =
    Int


type Fruit
    = Apple
    | Orange


type alias Game =
    { columns : Int
    , rows : Int
    , fruits : Dict FruitId Fruit
    , fields : Dict ( Int, Int ) FruitId
    , selected : Maybe ( Int, Int )
    }


empty : { columns : Int, rows : Int } -> Game
empty args =
    { columns = args.columns
    , rows = args.rows
    , fruits = Dict.empty
    , fields = Dict.empty
    , selected = Nothing
    }


addFruit : ( Int, Int ) -> Fruit -> Game -> ( Game, FruitId )
addFruit ( x, y ) fruit game =
    let
        fruitId =
            Dict.size game.fruits
    in
    ( { game
        | fruits = Dict.insert fruitId fruit game.fruits
        , fields = game.fields |> Dict.insert ( x, y ) fruitId
      }
    , fruitId
    )


getFruitAt : ( Int, Int ) -> Game -> Maybe Fruit
getFruitAt pos game =
    game.fields
        |> Dict.get pos
        |> Maybe.andThen
            (\fruitId ->
                Dict.get fruitId game.fruits
            )


setSelected : Maybe ( Int, Int ) -> Game -> Game
setSelected selected game =
    { game | selected = selected }


removeField : ( Int, Int ) -> Game -> Game
removeField pos game =
    { game | fields = Dict.remove pos game.fields }


isValidPair : ( Int, Int ) -> ( Int, Int ) -> Game -> Bool
isValidPair ( x1, y1 ) ( x2, y2 ) game =
    (((x1 == x2)
        && (List.range (min y1 y2 + 1) (max y1 y2 - 1)
                |> List.all
                    (\y ->
                        game.fields |> Dict.member ( x1, y ) |> not
                    )
           )
     )
        || ((y1 == y2)
                && (List.range (min x1 x2 + 1) (max x1 x2 - 1)
                        |> List.all
                            (\x ->
                                game.fields |> Dict.member ( x, y1 ) |> not
                            )
                   )
           )
    )
        && (getFruitAt ( x1, y1 ) game /= getFruitAt ( x2, y2 ) game)
