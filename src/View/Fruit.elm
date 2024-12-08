module View.Fruit exposing (..)

import Html exposing (Html)
import Html.Style
import Level exposing (BlockId, Entity, Level)
import View.Block
import View.Field


viewFruit : { blockId : BlockId, entity : Entity, pos : ( Int, Int ), game : Level } -> List (Html msg) -> Html msg
viewFruit args =
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
        )
