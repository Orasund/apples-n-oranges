module Screen.Menu exposing (..)

import Bag exposing (Bag)
import Data.Block exposing (Block(..), Optional)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Style
import Puzzle.Setting exposing (Event(..))
import View.Background
import View.Block
import View.Button
import View.CalenderDay
import View.Color
import View.DayOfTheWeek


type MenuTab
    = CalenderTab
    | BulletinTab


type alias Trade =
    { remove : List Optional, add : Optional }


pinboard :
    { show : Bool
    , trades : List Trade
    , onClose : msg
    , onSelectTab : MenuTab -> msg
    , onAcceptTrade : Trade -> msg
    , items : Bag
    }
    -> Html msg
pinboard args =
    args.trades
        |> List.map
            (\trade ->
                let
                    cost =
                        trade.remove
                            |> List.map
                                (\items ->
                                    items
                                        |> OptionalBlock
                                        |> Data.Block.toString
                                )
                            |> String.concat
                in
                [ trade.add
                    |> OptionalBlock
                    |> Data.Block.toString
                    |> Html.text
                    |> List.singleton
                    |> Html.div [ Html.Style.fontSizePx 70 ]
                , if
                    trade.remove
                        |> List.foldl Bag.insert Bag.empty
                        |> Bag.toList
                        |> List.all (\( item, n ) -> Bag.contains n item args.items)
                  then
                    View.Button.withIcons
                        { label = "Trade"
                        , onPress = args.onAcceptTrade trade
                        }
                        cost

                  else
                    "for "
                        ++ cost
                        |> Html.text
                        |> List.singleton
                        |> Html.div []
                ]
                    |> Html.div
                        [ Html.Style.displayFlex
                        , Html.Style.flexDirectionColumn
                        , Html.Style.alignItemsCenter
                        , Html.Style.justifyContentCenter
                        , Html.Style.gapPx 8
                        , Html.Style.flex "1"
                        , Html.Style.aspectRatio "1"
                        , Html.Style.borderRadiusPx 8
                        , Html.Style.paddingPx 8
                        ]
            )
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionRow
            , Html.Style.flexWrapWrap
            , Html.Style.gapPx 16
            , Html.Style.paddingPx 8
            ]
        |> List.singleton
        |> toHtml
            { show = args.show
            , selected = BulletinTab
            , onClose = args.onClose
            , onSelectTab = args.onSelectTab
            }


calender :
    { show : Bool
    , today : Int
    , summer : Bool
    , events : Dict Int Event
    , onClose : msg
    , onSelectTab : MenuTab -> msg
    }
    -> Html msg
calender args =
    [ (if args.summer then
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
                    View.Color.yellow300

                 else
                    View.Color.blue300
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
                                                    [ Html.Style.backgroundColor View.Color.red900
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
        |> toHtml
            { show = args.show
            , selected = CalenderTab
            , onClose = args.onClose
            , onSelectTab = args.onSelectTab
            }


toHtml :
    { show : Bool
    , onClose : msg
    , selected : MenuTab
    , onSelectTab : MenuTab -> msg
    }
    -> List (Html msg)
    -> Html msg
toHtml args content =
    [ [ ( CalenderTab, "Calender" )
      , ( BulletinTab, "Market" )
      ]
        |> List.map
            (\( tab, label ) ->
                (if tab == args.selected then
                    View.Button.fake

                 else
                    View.Button.toHtml
                )
                    { label = label
                    , onPress = args.onSelectTab tab
                    }
            )
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.gapPx 8
            ]
    , content
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
