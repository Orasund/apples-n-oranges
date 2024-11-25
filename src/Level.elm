module Level exposing (..)

import Bag exposing (Item(..))
import Dict exposing (Dict)
import Maths
import Random exposing (Generator)


type alias Random a =
    Generator a


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
    | Carrot


type Solid
    = Rock


type Optional
    = Dynamite
    | Fish
    | Rabbit


type Block
    = FruitBlock Fruit
    | FishingRod
    | OptionalBlock Optional
    | SolidBlock Solid


type alias CoinId =
    Int


type alias Entity =
    { x : Float
    , y : Float
    , shrink : Bool
    }


type alias Level =
    { columns : Int
    , rows : Int
    , blocks : Dict BlockId Block
    , fields : Dict ( Int, Int ) BlockId
    , selected : Maybe ( Int, Int )
    , entities :
        Dict
            BlockId
            { entity : Entity
            , pos : ( Int, Int )
            , item : Item
            }
    , items : Dict CoinId { entity : Entity, sort : Item }
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
    , items = Dict.empty
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
    , shrink = False
    }


blockToItem : Block -> Random Item
blockToItem block =
    case block of
        OptionalBlock Rabbit ->
            Random.uniform Diamant
                [ Stone
                ]

        _ ->
            Random.uniform Coin
                [ Worm
                , Snail
                , Stone
                ]


addBlock : ( Int, Int ) -> Block -> Level -> Random Level
addBlock ( x, y ) block game =
    let
        fruitId =
            Dict.size game.blocks
    in
    blockToItem block
        |> Random.map
            (\item ->
                { game
                    | blocks = Dict.insert fruitId block game.blocks
                    , fields = game.fields |> Dict.insert ( x, y ) fruitId
                    , entities =
                        Dict.insert fruitId
                            { entity = newEntity ( x, y )
                            , pos = ( x, y )
                            , item = item
                            }
                            game.entities
                }
            )


getBlockAt : ( Int, Int ) -> Level -> Maybe Block
getBlockAt pos game =
    game.fields
        |> Dict.get pos
        |> Maybe.andThen
            (\fruitId ->
                Dict.get fruitId game.blocks
            )


getEntityAndItem : ( Int, Int ) -> Level -> Maybe ( BlockId, ( Entity, Item ) )
getEntityAndItem pos game =
    game.fields
        |> Dict.get pos
        |> Maybe.andThen
            (\blockId ->
                Dict.get blockId game.entities
                    |> Maybe.map
                        (\args ->
                            ( blockId, ( args.entity, args.item ) )
                        )
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

                _ ->
                    [ ( OptionalBlock Dynamite, SolidBlock Rock )
                    , ( FishingRod, OptionalBlock Fish )
                    , ( OptionalBlock Rabbit, FruitBlock Carrot )
                    ]
                        |> List.any
                            (\pair ->
                                ( p1, p2 ) == pair || ( p2, p1 ) == pair
                            )
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


collectCoins : Level -> ( Level, List Item )
collectCoins model =
    let
        updateEntity entity =
            { entity | x = 2.5, y = -1 }
    in
    ( { model
        | items =
            Dict.map
                (\_ coin ->
                    { coin | entity = updateEntity coin.entity }
                )
                model.items
      }
    , model.items
        |> Dict.values
        |> List.map .sort
    )


addItem : ( Float, Float ) -> Item -> Level -> Level
addItem p item level =
    let
        ( x, y ) =
            fromPolar ( 0.2, toFloat level.nextCoinId * 2.5 )
                |> Maths.plus p
    in
    { level
        | items =
            level.items
                |> Dict.insert level.nextCoinId
                    { entity =
                        { x = x
                        , y = y
                        , shrink = True
                        }
                    , sort = item
                    }
        , nextCoinId = level.nextCoinId + 1
    }


showCoin : Int -> Level -> Level
showCoin coinId level =
    let
        updateEntity entity =
            { entity
                | shrink = False
            }
    in
    { level
        | items =
            Dict.update coinId
                (Maybe.map
                    (\coin ->
                        { coin | entity = updateEntity coin.entity }
                    )
                )
                level.items
    }


moveEntity : BlockId -> Entity -> Level -> Level
moveEntity blockId entity level =
    { level
        | entities =
            Dict.update blockId
                (Maybe.map
                    (\args ->
                        { args | entity = entity }
                    )
                )
                level.entities
    }
