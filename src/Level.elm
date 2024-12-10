module Level exposing (..)

import Data.Block exposing (Block(..), Item(..), Organic(..))
import Dict exposing (Dict)
import Maths
import Random exposing (Generator)
import Set exposing (Set)


type alias Random a =
    Generator a


type alias BlockId =
    Int


type alias Puzzle =
    { columns : Int
    , rows : Int
    , blocks : List ( ( Int, Int ), Block )
    , solids : Set ( Int, Int )
    }


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
            , item : Maybe Block
            }
    , pairs : Dict CoinId { entity : Entity, sort : Block }
    , solids : Set ( Int, Int )
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
    , pairs = Dict.empty
    , solids = Set.empty
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
            Dict.insert fruitId
                { entity = newEntity ( x, y )
                , pos = ( x, y )
                , item = Just block
                }
                game.entities
    }


getBlockAt : ( Int, Int ) -> Level -> Maybe Block
getBlockAt pos game =
    game.fields
        |> Dict.get pos
        |> Maybe.andThen
            (\fruitId ->
                Dict.get fruitId game.blocks
            )


getEntityAndItem : ( Int, Int ) -> Level -> Maybe ( BlockId, ( Entity, Maybe Block ) )
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
        isNotSolid pos =
            Dict.member pos game.fields
                || Set.member pos game.solids
    in
    (((x1 == x2)
        && (List.range (min y1 y2 + 1) (max y1 y2 - 1)
                |> List.all
                    (\y ->
                        isNotSolid ( x1, y ) |> not
                    )
           )
     )
        || ((y1 == y2)
                && (List.range (min x1 x2 + 1) (max x1 x2 - 1)
                        |> List.all
                            (\x ->
                                isNotSolid ( x, y1 ) |> not
                            )
                   )
           )
    )
        && (getBlockAt ( x1, y1 ) game /= getBlockAt ( x2, y2 ) game)
        && (Maybe.map2 Data.Block.isValidBlock
                (getBlockAt ( x1, y1 ) game)
                (getBlockAt ( x2, y2 ) game)
                |> Maybe.withDefault False
           )


getBlocks : Level -> Dict ( Int, Int ) Block
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
        |> Dict.fromList


addPartOfPair : ( Float, Float ) -> Block -> Level -> Level
addPartOfPair p item level =
    let
        ( x, y ) =
            fromPolar ( 0.1, toFloat level.nextCoinId * 2.5 )
                |> Maths.plus p
    in
    { level
        | pairs =
            level.pairs
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
        | pairs =
            Dict.update coinId
                (Maybe.map
                    (\coin ->
                        { coin | entity = updateEntity coin.entity }
                    )
                )
                level.pairs
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


replace : { search : List Item, replaceWith : List Item } -> Level -> Level
replace args level =
    let
        rec { out, search, replaceWith } remaining =
            case ( remaining, search ) of
                ( _, [] ) ->
                    remaining ++ out

                ( ( id, head ) :: list, nextSearch :: remainingSearch ) ->
                    if head == ItemBlock nextSearch then
                        case replaceWith of
                            nextReplace :: remainingReplace ->
                                rec
                                    { out = ( id, ItemBlock nextReplace ) :: out
                                    , search = remainingSearch
                                    , replaceWith = remainingReplace
                                    }
                                    list

                            [] ->
                                rec
                                    { out = out
                                    , search = remainingSearch
                                    , replaceWith = []
                                    }
                                    list

                    else
                        rec
                            { out = ( id, head ) :: out
                            , search = search
                            , replaceWith = replaceWith
                            }
                            list

                ( [], _ ) ->
                    out
    in
    { level
        | blocks =
            level.blocks
                |> Dict.toList
                |> rec
                    { out = []
                    , search = args.search
                    , replaceWith = args.replaceWith
                    }
                |> Dict.fromList
    }
