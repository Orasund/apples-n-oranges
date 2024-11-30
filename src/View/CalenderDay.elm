module View.CalenderDay exposing (..)

import Block
import Event exposing (Event(..))
import Html exposing (Attribute, Html)
import Html.Style


eventToString : Event -> String
eventToString event =
    case event of
        WeatherEvent weather ->
            weather.symbol
                |> Block.toString

        ShopEvent ->
            "🔮"


stylingForEvent : Float -> Event -> List (Attribute msg)
stylingForEvent calenderSize event =
    case event of
        WeatherEvent weather ->
            case weather.difficulty of
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

        ShopEvent ->
            []


calenderDay : Float -> List (Attribute msg) -> Event -> Html msg
calenderDay calenderSize attrs event =
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
        , eventToString event
            |> Html.text
            |> List.singleton
            |> Html.div
                (stylingForEvent calenderSize event
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
