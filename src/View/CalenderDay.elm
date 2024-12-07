module View.CalenderDay exposing (..)

import Data.Block
import Event exposing (Event(..))
import Html exposing (Attribute, Html)
import Html.Style


eventToString : Event -> Maybe String
eventToString event =
    case event of
        WeatherEvent weather ->
            weather.setting.symbol
                |> Maybe.map Data.Block.toString

        ShopEvent ->
            "🔮" |> Just


stylingForEvent : Float -> Event -> List (Attribute msg)
stylingForEvent calenderSize event =
    case event of
        WeatherEvent weather ->
            case weather.setting.difficulty of
                0 ->
                    []

                1 ->
                    []

                2 ->
                    [ Html.Style.borderWidthPx (calenderSize * 0.1)
                    , Html.Style.borderStyleDouble
                    , Html.Style.borderColor "#ddd"
                    ]

                3 ->
                    [ Html.Style.borderWidthPx (calenderSize * 0.1)
                    , Html.Style.borderStyleDouble
                    , Html.Style.borderColor "rgb(242 245 122)"
                    ]

                _ ->
                    [ Html.Style.borderWidthPx (calenderSize * 0.1)
                    , Html.Style.borderStyleSolid
                    , Html.Style.borderColor "rgb(242 245 122)"
                    ]

        ShopEvent ->
            []


difficutlyOfEvent : Event -> Int
difficutlyOfEvent event =
    case event of
        WeatherEvent weather ->
            weather.setting.difficulty

        ShopEvent ->
            0


calenderDay : { size : Float, day : Int } -> List (Attribute msg) -> Event -> Html msg
calenderDay args attrs event =
    Html.div
        ([ Html.Style.displayFlex
         , Html.Style.flexDirectionColumn
         , Html.Style.fontSizePx (args.size * 0.5)
         , Html.Style.backgroundColor "#fff"
         , Html.Style.borderRadiusPx (args.size * 0.1)
         , Html.Style.overflowHidden
         , Html.Style.heightPx args.size
         , Html.Style.widthPx args.size
         ]
            ++ attrs
        )
        [ "⭐"
            |> List.repeat (difficutlyOfEvent event)
            |> String.join " "
            |> Html.text
            |> List.singleton
            |> Html.span [ Html.Style.filter "grayscale(1) brightness(2)" ]
            |> List.singleton
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.justifyContentCenter
                , Html.Style.alignItemsCenter
                , Html.Style.backgroundColor "red"
                , Html.Style.heightPx (args.size / 4)
                , Html.Style.fontSizePx (args.size * 0.1)
                ]
        , eventToString event
            |> Maybe.withDefault (args.day |> String.fromInt)
            |> Html.text
            |> List.singleton
            |> Html.div
                (stylingForEvent args.size event
                    ++ [ Html.Style.displayFlex
                       , Html.Style.justifyContentCenter
                       , Html.Style.alignItemsCenter
                       , Html.Style.height "100%"
                       , Html.Style.boxSizingBorderBox
                       , Html.Style.borderBottomLeftRadiusPx (args.size * 0.1)
                       , Html.Style.borderBottomRightRadiusPx (args.size * 0.1)
                       ]
                )
        ]
