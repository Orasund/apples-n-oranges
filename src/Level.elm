module Level exposing (..)

import Dict exposing (Dict)


type alias BlockId =
    Int


type alias Puzzle =
    { columns : Int
    , rows : Int
    , blocks : List ( ( Int, Int ), Block )
    }


type Fruit
    = Apple
    | Orange
    | Lemon
    | Grapes


type Solid
    = Stone
    | Sprout
    | Dynamite


type Block
    = FruitBlock Fruit
    | SolidBlock Solid


type alias Level =
    { columns : Int
    , rows : Int
    , blocks : Dict BlockId Block
    , fields : Dict ( Int, Int ) BlockId
    , selected : Maybe ( Int, Int )
    }


empty : { columns : Int, rows : Int } -> Level
empty args =
    { columns = args.columns
    , rows = args.rows
    , blocks = Dict.empty
    , fields = Dict.empty
    , selected = Nothing
    }


clear : Level -> Level
clear level =
    empty { columns = level.columns, rows = level.rows }


addBlock : ( Int, Int ) -> Block -> Level -> ( Level, BlockId )
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


getBlockAt : ( Int, Int ) -> Level -> Maybe Block
getBlockAt pos game =
    game.fields
        |> Dict.get pos
        |> Maybe.andThen
            (\fruitId ->
                Dict.get fruitId game.blocks
            )


getBlockAndIdAt : ( Int, Int ) -> Level -> Maybe ( BlockId, Block )
getBlockAndIdAt pos game =
    game.fields
        |> Dict.get pos
        |> Maybe.andThen
            (\blockId ->
                Dict.get blockId game.blocks
                    |> Maybe.map (Tuple.pair blockId)
            )


setSelected : Maybe ( Int, Int ) -> Level -> Level
setSelected selected game =
    { game | selected = selected }


removeField : ( Int, Int ) -> Level -> Level
removeField pos game =
    { game | fields = Dict.remove pos game.fields }


isValidPair : ( Int, Int ) -> ( Int, Int ) -> Level -> Bool
isValidPair ( x1, y1 ) ( x2, y2 ) game =
    let
        isValidBlock p1 p2 =
            case ( p1, p2 ) of
                ( FruitBlock _, FruitBlock _ ) ->
                    True

                ( SolidBlock Dynamite, SolidBlock Stone ) ->
                    True

                ( SolidBlock Stone, SolidBlock Dynamite ) ->
                    True

                _ ->
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
        && (Maybe.map2 isValidBlock
                (getBlockAt ( x1, y1 ) game)
                (getBlockAt ( x2, y2 ) game)
                |> Maybe.withDefault False
           )


getBlocks : Level -> List ( ( Int, Int ), Block )
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
