module View.Shop exposing (..)

import Html exposing (Html)
import Html.Style
import Level.Generator
import View.Background
import View.CalenderDay


calenderSize =
    60


gap =
    16


columns =
    5


toHtml : Html msg
toHtml =
    Level.Generator.settings
        |> List.map
            (\setting ->
                View.CalenderDay.calenderDay calenderSize [] setting
            )
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionRow
            , Html.Style.flexWrapWrap
            , Html.Style.gapPx gap
            , Html.Style.widthPx (columns * calenderSize + (columns - 1) * gap)
            ]
        |> List.singleton
        |> View.Background.shop
            [ Html.Style.displayFlex
            , Html.Style.alignItemsCenter
            , Html.Style.justifyContentCenter
            , Html.Style.positionAbsolute
            , Html.Style.gapPx 16
            , Html.Style.width "100%"
            , Html.Style.height "100%"
            ]
