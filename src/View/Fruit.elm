module View.Fruit exposing (..)

import Data.Block exposing (Item)
import Html exposing (Attribute, Html)
import Html.Style
import Level exposing (BlockId, Entity, Level)
import View.Block
import View.Field


viewItem : { pos : ( Int, Int ), item : Item } -> List (Html msg) -> Html msg
viewItem args =
    let
        ( x, y ) =
            args.pos
    in
    View.Block.withContent
        [ Html.Style.topPx (toFloat y * View.Field.size)
        , Html.Style.leftPx (toFloat x * View.Field.size)
        ]


viewFruit : List (Attribute msg) -> { blockId : BlockId, entity : Entity, pos : ( Int, Int ), game : Level } -> List (Html msg) -> Html msg
viewFruit attrs args =
    View.Block.withContent
        ([ Html.Style.topPx (args.entity.y * View.Field.size)
         , Html.Style.leftPx (args.entity.x * View.Field.size)
         ]
            ++ (if args.entity.shrink then
                    [ View.Block.shrink ]

                else
                    args.game.selected
                        |> Maybe.map
                            (\selected ->
                                if selected == args.pos then
                                    [ View.Block.small ]

                                else if Level.isValidPair args.pos selected args.game then
                                    [ View.Block.rocking ]

                                else
                                    []
                            )
                        |> Maybe.withDefault []
               )
            ++ attrs
        )
