module View.Game exposing (..)

import Dict exposing (Dict)
import Entity exposing (Entity)
import Game exposing (BlockId, Game)
import Html exposing (Html)
import Html.Keyed
import Html.Style
import Layout
import View.Block
import View.Coin exposing (Coin, CoinId)
import View.Field
import View.Fruit


viewGame :
    { game : Game
    , coins : Dict CoinId Coin
    , entities : Dict BlockId Entity
    , onClick : ( Int, Int ) -> msg
    }
    -> Html msg
viewGame args =
    [ [ View.Field.toHtml
            [ View.Field.light ]
            { columns = args.game.columns
            , rows = args.game.rows
            }
      , [ args.entities
            |> Dict.toList
            |> List.filterMap
                (\( blockId, entity ) ->
                    args.game.blocks
                        |> Dict.get blockId
                        |> Maybe.map
                            (\block ->
                                { blockId = blockId
                                , entity = entity
                                , block = block
                                }
                            )
                )
            |> List.map
                (\{ blockId, entity, block } ->
                    ( "block_" ++ String.fromInt blockId
                    , block
                        |> View.Block.toString
                        |> Html.text
                        |> List.singleton
                        |> View.Fruit.viewFruit
                            { blockId = blockId
                            , entity = entity
                            , game = args.game
                            }
                    )
                )
        , args.coins
            |> Dict.toList
            |> List.singleton
            |> List.concat
            |> List.sortBy Tuple.first
            |> List.map
                (\( id, coin ) ->
                    ( "coin_" ++ String.fromInt id
                    , View.Coin.asBlock coin
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
        |> Html.div [ Html.Style.positionRelative ]
