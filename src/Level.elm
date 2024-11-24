module Level exposing (..)

import Dict exposing (Dict)
import Entity exposing (Entity)


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


type Optional
    = Dynamite
    | Fish


type Block
    = FruitBlock Fruit
    | FishingRod
    | OptionalBlock Optional
    | SolidBlock Solid


type alias CoinId =
    Int


type alias Coin =
    { x : Float, y : Float, shrink : Bool, value : Int }


type alias Level =
    { columns : Int
    , rows : Int
    , blocks : Dict BlockId Block
    , fields : Dict ( Int, Int ) BlockId
    , selected : Maybe ( Int, Int )
    , entities : Dict BlockId Entity
    , coins : Dict CoinId Coin
    , nextCoinId : CoinId
    }


empty : { columns : Int, rows : Int } -> Level
empty args =
    { columns = args.columns
    , rows = args.rows
    , blocks = Dict.empty
    , fields = Dict.empty
    , selected = Nothing
    , entities = Dict.empty
    , coins = Dict.empty
    , nextCoinId = 0
    }


clear : Level -> Level
clear level =
    empty
        { columns = level.columns
        , rows = level.rows
        }


newEntity : ( Int, Int ) -> Entity
newEntity ( x, y ) =
    { x = toFloat x
    , y = toFloat y
    , pos = ( x, y )
    , shrink = False
    }


addBlock : ( Int, Int ) -> Block -> Level -> Level
addBlock ( x, y ) block game =
    let
        fruitId =
            Dict.size game.blocks
    in
    { game
        | blocks = Dict.insert fruitId block game.blocks
        , fields = game.fields |> Dict.insert ( x, y ) fruitId
        , entities =
            game.entities
                |> Dict.insert fruitId (newEntity ( x, y ))
    }


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

                ( OptionalBlock Dynamite, SolidBlock Stone ) ->
                    True

                ( SolidBlock Stone, OptionalBlock Dynamite ) ->
                    True

                ( FishingRod, OptionalBlock Fish ) ->
                    True

                ( OptionalBlock Fish, FishingRod ) ->
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


collectCoins : Level -> ( Level, Int )
collectCoins model =
    ( { model
        | coins = Dict.map (\_ coin -> { coin | x = 2.5, y = -1 }) model.coins
      }
    , model.coins
        |> Dict.values
        |> List.map .value
        |> List.sum
    )


addCoin : ( Float, Float ) -> Int -> Level -> Level
addCoin ( x, y ) value level =
    { level
        | coins =
            level.coins
                |> Dict.insert level.nextCoinId
                    { x = x
                    , y = y
                    , shrink = True
                    , value = value
                    }
        , nextCoinId = level.nextCoinId + 1
    }


showCoin : Int -> Level -> Level
showCoin coinId level =
    { level
        | coins =
            Dict.update coinId
                (Maybe.map
                    (\coin ->
                        { coin
                            | shrink = False
                        }
                    )
                )
                level.coins
    }


moveEntity : BlockId -> { x : Float, y : Float, shrink : Bool } -> Level -> Level
moveEntity blockId args level =
    { level
        | entities =
            level.entities
                |> Dict.update blockId
                    (Maybe.map
                        (\entity ->
                            { entity | x = args.x, y = args.y, shrink = args.shrink }
                        )
                    )
    }
