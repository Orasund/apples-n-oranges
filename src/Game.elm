module Game exposing (..)

import Dict exposing (Dict)


type alias BlockId =
    Int


type Fruit
    = Apple
    | Orange


type Solid
    = Stone
    | Sprout


type Block
    = FruitBlock Fruit
    | SolidBlock Solid


type alias Game =
    { columns : Int
    , rows : Int
    , blocks : Dict BlockId Block
    , fields : Dict ( Int, Int ) BlockId
    , selected : Maybe ( Int, Int )
    }


empty : { columns : Int, rows : Int } -> Game
empty args =
    { columns = args.columns
    , rows = args.rows
    , blocks = Dict.empty
    , fields = Dict.empty
    , selected = Nothing
    }


addBlock : ( Int, Int ) -> Block -> Game -> ( Game, BlockId )
addBlock ( x, y ) block game =
    let
        fruitId =
            Dict.size game.blocks
    in
    ( { game
        | blocks = Dict.insert fruitId block game.blocks
        , fields = game.fields |> Dict.insert ( x, y ) fruitId
      }
    , fruitId
    )


getBlockAt : ( Int, Int ) -> Game -> Maybe Block
getBlockAt pos game =
    game.fields
        |> Dict.get pos
        |> Maybe.andThen
            (\fruitId ->
                Dict.get fruitId game.blocks
            )


getBlockAndIdAt : ( Int, Int ) -> Game -> Maybe ( BlockId, Block )
getBlockAndIdAt pos game =
    game.fields
        |> Dict.get pos
        |> Maybe.andThen
            (\blockId ->
                Dict.get blockId game.blocks
                    |> Maybe.map (Tuple.pair blockId)
            )


setSelected : Maybe ( Int, Int ) -> Game -> Game
setSelected selected game =
    { game | selected = selected }


removeField : ( Int, Int ) -> Game -> Game
removeField pos game =
    { game | fields = Dict.remove pos game.fields }


isValidPair : ( Int, Int ) -> ( Int, Int ) -> Game -> Bool
isValidPair ( x1, y1 ) ( x2, y2 ) game =
    let
        isValidBlock p =
            case p of
                FruitBlock _ ->
                    True

                SolidBlock _ ->
                    False
    in
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
        && (getBlockAt ( x1, y1 ) game /= getBlockAt ( x2, y2 ) game)
        && (getBlockAt ( x1, y1 ) game |> Maybe.map isValidBlock |> Maybe.withDefault False)
        && (getBlockAt ( x2, y2 ) game |> Maybe.map isValidBlock |> Maybe.withDefault False)


getBlocks : Game -> List ( ( Int, Int ), Block )
getBlocks game =
    game.fields
        |> Dict.map
            (\_ id ->
                game.blocks |> Dict.get id
            )
        |> Dict.toList
        |> List.filterMap
            (\( pos, maybeBlock ) ->
                maybeBlock |> Maybe.map (Tuple.pair pos)
            )
