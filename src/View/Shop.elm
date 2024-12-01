module View.Shop exposing (..)

import Bag
import Data.Block
import Event exposing (Event(..))
import Html exposing (Html)
import Html.Style
import Layout
import Puzzle.Setting exposing (Setting)
import View.Background
import View.Block
import View.Button
import View.CalenderDay
import View.DayOfTheWeek


calenderSize =
    60


gap =
    16


columns =
    3


toHtml :
    { settings : List Setting
    , buyableSettings : List Setting
    , onSelectSettingToBuy : Maybe Int -> msg
    , selected : Maybe Int
    , onBuy : Int -> msg
    }
    -> Html msg
toHtml args =
    [ [ args.buyableSettings
            |> List.indexedMap
                (\i weather ->
                    [ View.CalenderDay.calenderDay calenderSize
                        (Html.Style.transition "scale 0.2s ease-in"
                            :: Layout.asButton
                                { label = "Select"
                                , onPress =
                                    args.onSelectSettingToBuy
                                        (if args.selected == Just i then
                                            Nothing

                                         else
                                            Just i
                                        )
                                        |> Just
                                }
                            ++ (case args.selected of
                                    Nothing ->
                                        [ View.Block.rocking ]

                                    Just selected ->
                                        if selected == i then
                                            [ View.Block.small ]

                                        else
                                            [ Html.Style.scale "1.0" ]
                               )
                        )
                        (WeatherEvent weather)
                    , (weather
                        |> Puzzle.Setting.toBag
                        |> Bag.toList
                      )
                        |> List.map
                            (\( block, amount ) ->
                                [ String.fromInt amount
                                    ++ " "
                                    ++ Data.Block.toString block
                                    |> Html.text
                                ]
                                    |> Html.div
                                        [ Html.Style.padding "4px 8px"
                                        , Html.Style.background "white"
                                        , Html.Style.borderRadiusPx 16
                                        ]
                            )
                        |> Html.div
                            [ Html.Style.displayFlex
                            , Html.Style.flexDirectionRow
                            , Html.Style.flexWrapWrap
                            , Html.Style.justifyContentCenter
                            , Html.Style.gapPx 4
                            , Html.Style.fontSizePx 12
                            ]
                    , if args.selected == Just i then
                        View.Button.toHtml
                            { label = "Cancel"
                            , onPress = args.onSelectSettingToBuy Nothing
                            }

                      else
                        View.Button.toHtml
                            { label = "Select"
                            , onPress = args.onSelectSettingToBuy (Just i)
                            }
                    ]
                        |> Html.div
                            [ Html.Style.displayFlex
                            , Html.Style.flexDirectionColumn
                            , Html.Style.alignItemsCenter
                            , Html.Style.gapPx 8
                            , Html.Style.widthPx 130
                            ]
                )
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.flexDirectionRow
                , Html.Style.gapPx 16
                , Html.Style.justifyContentSpaceBetween
                ]
      , [ Html.div
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
        , args.settings
            |> List.indexedMap
                (\i weather ->
                    [ Html.text (View.DayOfTheWeek.toLongString (i + 1))
                    , View.CalenderDay.calenderDay calenderSize
                        (Layout.asButton
                            { label = "Select"
                            , onPress =
                                if args.selected == Nothing then
                                    Nothing

                                else
                                    args.onBuy i |> Just
                            }
                            ++ (if args.selected == Nothing then
                                    []

                                else
                                    [ View.Block.rocking ]
                               )
                        )
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
                ]
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
