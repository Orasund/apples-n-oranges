module View.Level exposing (..)

import Data.Block exposing (Block(..))
import Dict
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Touch as Touch
import Html.Keyed
import Html.Style
import ItemBag exposing (ItemBag)
import Level exposing (Level)
import Set
import View.Coin
import View.Field
import View.Fruit


noEvents : Attribute msg
noEvents =
    Html.Attributes.class "no-events"


calcCell : ( Float, Float ) -> ( Float, Float )
calcCell ( x, y ) =
    ( x / View.Field.size, y / View.Field.size )


viewGame :
    { game : Level
    , items : ItemBag
    , onPointerDown : { pos : ( Float, Float ), offset : ( Float, Float ) } -> msg
    , onPointerUp : ( Float, Float ) -> msg
    , onPointerEnd : ( Float, Float ) -> msg
    , zero : ( Float, Float )
    }
    -> Html msg
viewGame args =
    [ [ View.Field.toHtml
            [ View.Field.light
            , noEvents
            ]
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
                            []
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

    {--, args.game.fields
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
                    {--Layout.asButton
                        { onPress = Just (args.onClick ( x, y ))
                        , label =
                            [ "Select "
                            , String.fromInt x
                            , ", "
                            , String.fromInt y
                            ]
                                |> String.concat
                        }
                        ++--}
                    [ Html.Style.aspectRatio "1"
                    , Html.Style.widthPx View.Field.size
                    , Html.Style.positionAbsolute
                    , Html.Style.topPx (toFloat y * View.Field.size)
                    , Html.Style.leftPx (toFloat x * View.Field.size)
                    ]
                    []
            )--}
    , [ Html.div
            [ Html.Style.positionAbsolute
            , Pointer.onDown
                (\event ->
                    let
                        ( relX, relY ) =
                            event.pointer.offsetPos

                        ( absX, absY ) =
                            event.pointer.clientPos
                    in
                    args.onPointerDown
                        { offset = ( absX - relX, absY - relY )
                        , pos = calcCell event.pointer.offsetPos
                        }
                )

            --, Pointer.onMove (\event -> args.onPointerMove (calcCell event.pointer.offsetPos) |> Debug.log "move")
            , Pointer.onUp (\event -> args.onPointerUp (calcCell event.pointer.offsetPos) |> Debug.log "up")
            , Touch.onEnd
                (\event ->
                    let
                        ( zeroX, zeroY ) =
                            args.zero

                        ( absX, absY ) =
                            event.changedTouches
                                |> Debug.log "touchs"
                                |> List.head
                                |> Maybe.map (\touch -> touch.clientPos)
                                |> Maybe.withDefault ( 0, 0 )

                        ( relX, relY ) =
                            ( absX - zeroX, absY - zeroY )
                    in
                    args.onPointerUp (calcCell ( relX, relY ))
                )
            , Html.Style.widthPx (6 * View.Field.size)
            , Html.Style.heightPx (6 * View.Field.size)
            , Html.Style.topPx 0
            , Html.Style.leftPx 0
            , Html.Style.pointerEventsAll
            ]
            []
      ]
    ]
        |> List.concat
        |> Html.div
            [ Html.Style.positionRelative
            , Html.Style.overflowHidden
            , noEvents
            ]
