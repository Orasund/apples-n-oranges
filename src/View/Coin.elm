module View.Coin exposing (..)

import Data.Block exposing (Block)
import Html exposing (Attribute, Html)
import Html.Style
import Level exposing (Entity)
import View.Block
import View.Field


asBlock : Entity -> Block -> Html msg
asBlock entity item =
    View.Block.withContent
        ([ Html.Style.topPx (entity.y * View.Field.size)
         , Html.Style.leftPx (entity.x * View.Field.size)
         , Html.Style.heightPx View.Field.size
         ]
            ++ (if entity.shrink then
                    [ View.Block.shrink ]

                else
                    []
               )
        )
        [ item
            |> Data.Block.toString
            |> Html.text
            |> List.singleton
            |> Html.div [ Html.Style.fontSizePx (View.Field.size / 3) ]
        ]


toHtml : List (Attribute msg) -> Int -> Html msg
toHtml attrs n =
    String.fromInt n
        |> Html.text
        |> List.singleton
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.alignItemsCenter
            ]
        |> List.singleton
        |> Html.div
            ([ Html.Style.borderRadius "100%"
             , Html.Style.border "2px solid #eae649"
             , Html.Style.aspectRatio "1"
             , Html.Style.displayFlex
             , Html.Style.justifyContentCenter
             , Html.Style.alignContentCenter
             , Html.Style.boxSizingBorderBox
             , Html.Style.fontWeightBold
             , Html.Style.backgroundColor "#d8d42a"
             ]
                ++ attrs
            )
