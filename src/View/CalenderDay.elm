module View.CalenderDay exposing (..)

import Html exposing (Attribute, Html)
import Html.Style
import Puzzle.Generator exposing (Setting)
import View.Block


calenderDay : Float -> List (Attribute msg) -> Setting -> Html msg
calenderDay calenderSize attrs setting =
    Html.div
        ([ Html.Style.displayFlex
         , Html.Style.flexDirectionColumn
         , Html.Style.fontSizePx (calenderSize * 0.5)
         , Html.Style.backgroundColor "#fff"
         , Html.Style.borderRadiusPx (calenderSize * 0.1)
         , Html.Style.overflowHidden
         , Html.Style.heightPx calenderSize
         , Html.Style.widthPx calenderSize
         ]
            ++ attrs
        )
        [ Html.div
            [ Html.Style.backgroundColor "red"
            , Html.Style.heightPx (calenderSize / 5)
            ]
            []
        , setting.symbol
            |> View.Block.toString
            |> Html.text
            |> List.singleton
            |> Html.div
                ((case setting.difficulty of
                    0 ->
                        [ Html.Style.filter "contrast(0) brightness(1.5)"
                        ]

                    1 ->
                        []

                    2 ->
                        [ Html.Style.borderWidthPx (calenderSize * 0.1)
                        , Html.Style.borderStyleDouble
                        , Html.Style.borderColor "#ddd"
                        ]

                    _ ->
                        [ Html.Style.borderWidthPx (calenderSize * 0.1)
                        , Html.Style.borderStyleDouble
                        , Html.Style.borderColor "rgb(242 245 122)"
                        ]
                 )
                    ++ [ Html.Style.displayFlex
                       , Html.Style.justifyContentCenter
                       , Html.Style.alignItemsCenter
                       , Html.Style.height "100%"
                       , Html.Style.boxSizingBorderBox
                       , Html.Style.borderBottomLeftRadiusPx (calenderSize * 0.1)
                       , Html.Style.borderBottomRightRadiusPx (calenderSize * 0.1)
                       ]
                )
        ]
