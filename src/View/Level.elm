module View.Level exposing (..)

import Data.Block exposing (Block(..))
import Data.ItemBag exposing (ItemBag)
import Dict
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Touch as Touch
import Html.Keyed
import Html.Style
import Level exposing (Level)
import Set
import View.Field
import View.Fruit
import View.Pair


noEvents : Attribute msg
noEvents =
    Html.Attributes.class "no-events"


calcCell : ( Float, Float ) -> ( Float, Float )
calcCell ( x, y ) =
    ( x / View.Field.size, y / View.Field.size )


toHtml :
    { game : Level
    , items : ItemBag
    , onPointerDown : { pos : ( Float, Float ), offset : ( Float, Float ) } -> msg
    , onPointerUp : ( Float, Float ) -> msg
    , onPointerEnd : ( Float, Float ) -> msg
    , zero : ( Float, Float )
    }
    -> Html msg
toHtml args =
    [ [ View.Field.toHtml
            [ noEvents
            ]
            { columns = args.game.columns
            , rows = args.game.rows
            , holes = args.game.solids
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
            |> Data.ItemBag.toList
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
                    , View.Pair.asBlock entity sort
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
                        |> Debug.log "down"
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
                                |> List.head
                                |> Maybe.map (\touch -> touch.clientPos)
                                |> Maybe.withDefault ( 0, 0 )

                        ( relX, relY ) =
                            ( absX - zeroX, absY - zeroY )
                    in
                    args.onPointerUp (calcCell ( relX, relY ))
                        |> Debug.log "end"
                )
            , Html.Style.widthPx (toFloat args.game.columns * View.Field.size)
            , Html.Style.heightPx (toFloat args.game.rows * View.Field.size)
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
