module Screen.Menu exposing (..)

import Data.Block exposing (Block(..))
import Dict exposing (Dict)
import Event exposing (Event(..))
import Html exposing (Html)
import Html.Style
import View.Background
import View.Block
import View.Button
import View.CalenderDay
import View.Color
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
    [ View.Button.toHtml
        { label = "Calender"
        , onPress = args.onClose
        }
    , [ (if args.summer then
            "Summer"

         else
            "Winter"
        )
            |> Html.text
            |> List.singleton
            |> Html.div
                [ Html.Style.fontSizePx 32
                , Html.Style.height "100%"
                , Html.Style.displayFlex
                , Html.Style.justifyContentCenter
                , Html.Style.alignItemsCenter
                , Html.Style.borderTopLeftRadiusPx 8
                , Html.Style.borderTopRightRadiusPx 8
                , Html.Style.backgroundColor
                    (if args.summer then
                        "#fbfb74"

                     else
                        View.Color.blue200
                    )
                ]
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
                                        (if n == args.today then
                                            [ Html.Style.color "white"
                                            , View.Block.white
                                            ]

                                         else
                                            []
                                        )
                                    |> List.singleton
                                    |> Html.div
                                        ([ Html.Style.textAlignCenter
                                         , Html.Style.borderRadius "100%"
                                         , Html.Style.paddingPx 4
                                         , Html.Style.aspectRatio "1"
                                         , Html.Style.fontSizePx 20
                                         ]
                                            ++ (if n == args.today then
                                                    [ Html.Style.backgroundColor "red"
                                                    , Html.Style.fontWeightBold
                                                    ]

                                                else
                                                    []
                                               )
                                        )
                                , (case event of
                                    WeatherEvent weather ->
                                        weather.reward
                                            |> Maybe.map (\optional -> Data.Block.toString (OptionalBlock optional))
                                            |> Maybe.withDefault ""
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
                            , Html.Style.justifyContentSpaceBetween
                            , Html.Style.paddingPx 4
                            , Html.Style.aspectRatio "1"
                            ]
                )
        ]
            |> List.concat
            |> Html.div
                [ Html.Style.displayGrid
                , Html.Style.gridTemplateColumns "repeat(7,1fr)"
                ]
      ]
        |> Html.div
            [ Html.Style.backgroundColor "white"
            , Html.Style.widthPx 360
            , Html.Style.heightPx 360
            , Html.Style.boxSizingBorderBox
            , Html.Style.paddingPx 8
            , Html.Style.borderRadiusPx 16
            , Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.gapPx 8
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
            , Html.Style.width "100%"
            , Html.Style.height "100%"
            ]
