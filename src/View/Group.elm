module View.Group exposing (..)

import Block
import Html exposing (Html)
import Html.Style
import Puzzle.Builder exposing (Group(..))


toHtml : Group -> Html msg
toHtml group =
    (case group of
        Pair b1 b2 ->
            [ Html.text (Block.toString b2)
                |> List.singleton
                |> Html.div
                    [ Html.Style.fontSizePx 8
                    , Html.Style.positionAbsolute
                    , Html.Style.bottomPx -2
                    , Html.Style.leftPx 2
                    ]
            , Html.text (Block.toString b1)
                |> List.singleton
                |> Html.div
                    [ Html.Style.fontSizePx 14
                    , Html.Style.positionAbsolute
                    , Html.Style.topPx -6
                    , Html.Style.rightPx -4
                    ]
            ]
                |> Html.div [ Html.Style.positionRelative ]

        SingleBlock b ->
            Html.text (Block.toString b)
                |> List.singleton
                |> Html.div [ Html.Style.fontSizePx 16 ]
    )
        |> List.singleton
        |> Html.div
            [ Html.Style.backgroundColor "white"
            , Html.Style.borderRadiusPx 16
            , Html.Style.heightPx 32
            , Html.Style.widthPx 32
            , Html.Style.displayFlex
            , Html.Style.justifyContentCenter
            , Html.Style.alignItemsCenter
            ]
