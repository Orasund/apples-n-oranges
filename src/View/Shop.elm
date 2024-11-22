module View.Shop exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Style
import Layout
import Level.Generator exposing (Setting)
import View.Background
import View.Block
import View.Button
import View.CalenderDay
import View.Coin
import View.Header


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
    , money : Int

    --, onCloseShop : msg
    , onBuy : Int -> msg
    }
    -> Html msg
toHtml args =
    [ Html.div
        [ Html.Style.displayFlex
        , Html.Style.alignItemsCenter
        , Html.Style.justifyContentCenter
        , Html.Style.gapPx 4
        , Html.Style.widthPx 360
        ]
        [ []
            |> Html.div [ Html.Style.flex "1", Html.Style.displayFlex ]
        , View.Coin.toHtml
            [ Html.Style.fontSizePx 48
            , Html.Style.heightPx 100
            , Html.Style.borderWidthPx 8
            ]
            (args.money |> min 999 |> max -99)
            |> List.singleton
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                , Html.Style.justifyContentCenter
                ]
        , {--View.Button.toHtml
            { label = "Close"
            , onPress = args.onCloseShop
            }
            |> List.singleton--}
          []
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                , Html.Style.justifyContentEnd
                ]
        ]
    , [ args.buyableSettings
            |> List.indexedMap
                (\i setting ->
                    [ View.CalenderDay.calenderDay calenderSize
                        ([ Html.Style.transition "scale 0.2s ease-in" ]
                            ++ Layout.asButton
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
                        setting
                    , if args.selected == Just i then
                        View.Button.toHtml
                            { label = "Cancel"
                            , onPress = args.onSelectSettingToBuy Nothing
                            }

                      else
                        View.Button.withPrice
                            { price = Level.Generator.priceForSetting setting
                            , label = "Buy"
                            , onPress = args.onSelectSettingToBuy (Just i)
                            }
                    ]
                        |> Html.div
                            [ Html.Style.displayFlex
                            , Html.Style.flexDirectionColumn
                            , Html.Style.alignItemsCenter
                            , Html.Style.gapPx 8
                            ]
                )
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.flexDirectionRow
                , Html.Style.widthPx 150
                , Html.Style.justifyContentSpaceBetween
                ]
      , args.settings
            |> List.indexedMap
                (\i setting ->
                    View.CalenderDay.calenderDay calenderSize
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
                        setting
                )
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.flexDirectionRow
                , Html.Style.flexWrapWrap
                , Html.Style.gapPx gap
                , Html.Style.widthPx (columns * calenderSize + (columns - 1) * gap)
                ]
      ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.gapPx 16
            , Html.Style.widthPx 360
            , Html.Style.heightPx 360
            , Html.Style.alignItemsCenter
            ]
    ]
        |> View.Background.shop
            [ Html.Style.displayFlex
            , Html.Style.alignItemsCenter
            , Html.Style.justifyContentCenter
            , Html.Style.positionAbsolute
            , Html.Style.gapPx 32
            , Html.Style.width "100%"
            , Html.Style.height "100%"
            ]
