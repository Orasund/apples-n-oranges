module View.Calender exposing (..)

import Dict exposing (Dict)
import Event exposing (Event(..))
import Html exposing (Html)
import Html.Style
import View.Background
import View.Button
import View.CalenderDay
import View.DayOfTheWeek


toHtml :
    { show : Bool
    , today : Int
    , summer : Bool
    , events : Dict Int Event
    , onClose : msg
    }
    -> Html msg
toHtml args =
    [ (if args.summer then
        "Summer"

       else
        "Winter"
      )
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 32 ]
    , [ List.range 1 7
            |> List.map
                (\n ->
                    View.DayOfTheWeek.toShortString n
                        |> Html.text
                        |> List.singleton
                        |> Html.div
                            [ Html.Style.textAlignCenter
                            , Html.Style.paddingBottomPx 8
                            , Html.Style.borderBottom "1px solid black"
                            ]
                )
      , List.range 1 28
            |> List.map
                (\n ->
                    Dict.get n args.events
                        |> Maybe.map
                            (\event ->
                                [ View.CalenderDay.eventToString event
                                    |> Maybe.withDefault (String.fromInt n)
                                    |> Html.text
                                    |> List.singleton
                                    |> Html.div
                                        ([ Html.Style.textAlignCenter
                                         , Html.Style.borderRadius "100%"
                                         , Html.Style.paddingPx 4
                                         , Html.Style.aspectRatio "1"
                                         ]
                                            ++ (if n == args.today then
                                                    [ Html.Style.backgroundColor "red"
                                                    , Html.Style.color "white"
                                                    ]

                                                else
                                                    []
                                               )
                                        )
                                , (case event of
                                    WeatherEvent weather ->
                                        List.repeat weather.difficulty "⭐"
                                            |> String.concat

                                    ShopEvent ->
                                        ""
                                  )
                                    |> Html.text
                                    |> List.singleton
                                    |> Html.div [ Html.Style.fontSizePx 8 ]
                                ]
                            )
                        |> Maybe.withDefault []
                        |> Html.div
                            [ Html.Style.positionRelative
                            , Html.Style.displayFlex
                            , Html.Style.flexDirectionColumn
                            , Html.Style.alignItemsCenter
                            , Html.Style.justifyContentStart
                            , Html.Style.paddingPx 4
                            , Html.Style.aspectRatio "1"
                            ]
                )
      ]
        |> List.concat
        |> Html.div
            [ Html.Style.displayGrid
            , Html.Style.gridTemplateColumns "repeat(7,1fr)"
            , Html.Style.backgroundColor "white"
            , Html.Style.widthPx 360
            , Html.Style.boxSizingBorderBox
            , Html.Style.paddingPx 8
            , Html.Style.borderRadiusPx 8
            ]
    , View.Button.toHtml
        { label = "Close"
        , onPress = args.onClose
        }
    ]
        |> View.Background.wood
            [ Html.Style.displayFlex
            , Html.Style.alignItemsCenter
            , Html.Style.justifyContentCenter
            , Html.Style.positionAbsolute
            , Html.Style.transition "bottom 1s"
            , Html.Style.bottom
                (if args.show then
                    "0vh"

                 else
                    "100vh"
                )
            , Html.Style.gapPx 50
            , Html.Style.width "100%"
            , Html.Style.height "100%"
            ]
