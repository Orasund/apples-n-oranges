module View.Game exposing (..)

import Data.Block exposing (Block(..))
import Dict
import Html exposing (Html)
import Html.Events.Extra.Pointer as Pointer
import Html.Keyed
import Html.Style
import ItemBag exposing (ItemBag)
import Layout
import Level exposing (Level)
import Set
import View.Coin
import View.Field
import View.Fruit


viewGame :
    { game : Level
    , items : ItemBag
    , onClick : ( Int, Int ) -> msg
    }
    -> Html msg
viewGame args =
    [ [ View.Field.toHtml
            [ View.Field.light ]
            { columns = args.game.columns
            , rows = args.game.rows
            }
      , [ args.game.entities
            |> Dict.toList
            |> List.filterMap
                (\( blockId, { entity, pos } ) ->
                    args.game.blocks
                        |> Dict.get blockId
                        |> Maybe.map
                            (\block ->
                                { blockId = blockId
                                , entity = entity
                                , block = block
                                , pos = pos
                                }
                            )
                )
            |> List.map
                (\{ blockId, entity, block, pos } ->
                    ( "block_" ++ String.fromInt blockId
                    , block
                        |> Data.Block.toString
                        |> Html.text
                        |> List.singleton
                        |> View.Fruit.viewFruit
                            { blockId = blockId
                            , entity = entity
                            , pos = pos
                            , game = args.game
                            }
                    )
                )
        , args.items
            |> ItemBag.toList
            |> List.concatMap
                (\( item, set ) ->
                    set
                        |> Set.toList
                        |> List.map
                            (\( x, y ) ->
                                ( "item_" ++ String.fromInt x ++ "_" ++ String.fromInt y
                                , item
                                    |> ItemBlock
                                    |> Data.Block.toString
                                    |> Html.text
                                    |> List.singleton
                                    |> View.Fruit.viewItem
                                        { pos = ( x, y )
                                        , item = item
                                        }
                                )
                            )
                )
        , args.game.pairs
            |> Dict.toList
            |> List.singleton
            |> List.concat
            |> List.sortBy Tuple.first
            |> List.map
                (\( id, { entity, sort } ) ->
                    ( "pair_" ++ String.fromInt id
                    , View.Coin.asBlock entity sort
                    )
                )
        ]
            |> List.concat
            |> Html.Keyed.node "div"
                [ Html.Style.positionAbsolute
                , Html.Style.topPx 0
                , Html.Style.leftPx 0
                ]
      ]
    , args.game.fields
        |> Dict.toList
        |> List.filterMap
            (\( p, fruitId ) ->
                args.game.blocks
                    |> Dict.get fruitId
                    |> Maybe.map (Tuple.pair p)
            )
        |> List.map
            (\( ( x, y ), _ ) ->
                Html.div
                    (Layout.asButton
                        { onPress = Just (args.onClick ( x, y ))
                        , label =
                            [ "Select "
                            , String.fromInt x
                            , ", "
                            , String.fromInt y
                            ]
                                |> String.concat
                        }
                        ++ [ Html.Style.aspectRatio "1"
                           , Html.Style.widthPx View.Field.size
                           , Html.Style.positionAbsolute
                           , Html.Style.topPx (toFloat y * View.Field.size)
                           , Html.Style.leftPx (toFloat x * View.Field.size)
                           ]
                    )
                    []
            )
    ]
        |> List.concat
        |> Html.div
            [ Html.Style.positionRelative
            , Html.Style.overflowHidden

            {--, Pointer.onDown
                (\event ->
                    let
                        ( x, y ) =
                            event.pointer.offsetPos
                    in
                    args.onClick ( x / View.Field.size |> round, y / View.Field.size |> round )
                )--}
            ]
