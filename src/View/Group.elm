module View.Group exposing (..)

import Data.Block
import Html exposing (Html)
import Html.Style
import Puzzle.Builder exposing (Group(..))


toHtml : Group -> Html msg
toHtml group =
    (case group of
        Pair b1 b2 ->
            [ Html.text (Data.Block.toString b2)
            , Html.text (Data.Block.toString b1)
            ]

        SingleBlock b ->
            Html.text (Data.Block.toString b)
                |> List.singleton
    )
        |> Html.div
            [ Html.Style.backgroundColor "white"
            , Html.Style.borderRadiusPx 32
            , Html.Style.displayFlex
            , Html.Style.justifyContentCenter
            , Html.Style.alignItemsCenter
            , Html.Style.paddingPx 8
            , Html.Style.fontSizePx 24
            , Html.Style.gapPx 8
            ]
