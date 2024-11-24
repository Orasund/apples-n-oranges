module View.Coin exposing (..)

import Html exposing (Attribute, Html)
import Html.Style
import Level exposing (Coin)
import View.Block
import View.Field


asBlock : Coin -> Html msg
asBlock money =
    View.Block.withContent
        ([ Html.Style.topPx (money.y * View.Field.size)
         , Html.Style.leftPx (money.x * View.Field.size)
         , Html.Style.heightPx View.Field.size
         ]
            ++ (if money.shrink then
                    [ View.Block.shrink ]

                else
                    []
               )
        )
        [ toHtml
            [ Html.Style.fontSizePx (View.Field.size / 4)
            , Html.Style.heightPx (View.Field.size / 2)
            , Html.Style.borderWidthPx 4
            , Html.Style.displayFlex
            ]
            money.value
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
