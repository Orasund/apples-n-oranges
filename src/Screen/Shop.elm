module Screen.Shop exposing (..)

import Event exposing (Event(..))
import Html exposing (Html)
import Html.Style
import View.Background
import View.Button


calenderSize =
    60


gap =
    16


columns =
    3


toHtml :
    { onClose : msg
    }
    -> Html msg
toHtml args =
    [ [ [ Html.div
            [ Html.Style.positionAbsolute
            , Html.Style.topPx 0
            , Html.Style.textAlignCenter
            , Html.Style.padding "8px 32px"
            , Html.Style.borderRadiusPx 16
            , Html.Style.backgroundColor "#7e3395"
            , Html.Style.color "white"
            , Html.Style.fontWeightBold
            , Html.Style.transform "translate(0,-50%)"
            ]
            [ Html.text "Forcast" ]
        , Html.div
            [ Html.Style.positionAbsolute
            , Html.Style.topPx -40
            , Html.Style.leftPx -40
            , Html.Style.heightPx 80
            , Html.Style.widthPx 80
            , Html.Style.fontSizePx 64
            , Html.Style.textAlignCenter
            ]
            [ Html.text "🔮" ]

        {--, args.settings
            |> List.indexedMap
                (\i weather ->
                    [ Html.text (View.DayOfTheWeek.toLongString (i + 1))
                    , View.CalenderDay.calenderDay calenderSize
                        []
                        (WeatherEvent weather)
                    ]
                        |> Html.div
                            [ Html.Style.displayFlex
                            , Html.Style.flexDirectionColumn
                            , Html.Style.alignItemsCenter
                            , Html.Style.widthPx 80
                            , Html.Style.color "white"
                            , Html.Style.gapPx 4
                            , Html.Style.fontWeightBold
                            ]
                )
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.flexDirectionRow
                , Html.Style.flexWrapWrap
                , Html.Style.gridRowGapPx gap
                , Html.Style.widthPx 240
                ]--}
        ]
            |> View.Background.fortuneTeller
                [ Html.Style.positionRelative
                , Html.Style.alignItemsCenter
                , Html.Style.paddingPx 16
                , Html.Style.paddingTopPx 32
                , Html.Style.borderRadiusPx 16
                , Html.Style.boxSizingBorderBox
                ]
            |> List.singleton
            |> Html.div []
      , View.Button.toHtml
            { label = "Close"
            , onPress = args.onClose
            }
      , Html.text "Replace a day in the forcast to start the next week"
            |> List.singleton
            |> Html.div
                [ Html.Style.padding "8px 16px"
                , Html.Style.background "white"
                , Html.Style.borderRadiusPx 32
                ]
      ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.gapPx 32
            , Html.Style.widthPx 360
            , Html.Style.heightPx 360
            , Html.Style.alignItemsCenter
            ]
    ]
        |> View.Background.wood
            [ Html.Style.displayFlex
            , Html.Style.alignItemsCenter
            , Html.Style.justifyContentCenter
            , Html.Style.positionAbsolute
            , Html.Style.gapPx 32
            , Html.Style.width "100%"
            , Html.Style.height "100%"
            ]
