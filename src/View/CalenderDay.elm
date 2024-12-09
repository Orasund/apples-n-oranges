module View.CalenderDay exposing (..)

import Data.Block
import Html exposing (Attribute, Html)
import Html.Style
import Puzzle.Setting exposing (Event(..))
import View.Block
import View.Color


eventToString : Event -> Maybe String
eventToString event =
    case event of
        WeatherEvent weather ->
            weather.setting.symbol
                |> Maybe.map Data.Block.toString


stylingForEvent : Float -> Event -> List (Attribute msg)
stylingForEvent calenderSize event =
    case difficutlyOfEvent event of
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


difficutlyOfEvent : Event -> Int
difficutlyOfEvent event =
    case event of
        WeatherEvent weather ->
            weather.setting.difficulty


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
            |> Html.span [ View.Block.white ]
            |> List.singleton
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.justifyContentCenter
                , Html.Style.alignItemsCenter
                , Html.Style.backgroundColor View.Color.red900
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
