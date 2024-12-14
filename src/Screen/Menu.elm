module Screen.Menu exposing (..)

import Data.Block exposing (Block(..), Item(..))
import Data.Date as Date exposing (Date)
import Data.ItemBag exposing (ItemBag)
import Data.Mail exposing (Mail)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Style
import Puzzle.Setting exposing (Event)
import View.Background
import View.Block
import View.Button
import View.CalenderDay
import View.Color
import View.DayOfTheWeek


type MenuTab
    = CalenderTab
    | MarketTab
    | MailTab


type alias Trade =
    { remove : List ( Item, Int )
    , add : Item
    , trader : String
    }


messages :
    { show : Bool
    , mails : Dict Date Mail
    , onClose : msg
    , onAccept : Date -> msg
    , onSelectTab : MenuTab -> msg
    }
    -> Html msg
messages args =
    args.mails
        |> Dict.toList
        |> List.map
            (\( i, mail ) ->
                [ mail.sender
                    |> Html.text
                    |> List.singleton
                    |> Html.div
                        [ Html.Style.displayFlex
                        , Html.Style.alignItemsCenter
                        , Html.Style.justifyContentCenter
                        , Html.Style.textAlignCenter
                        , Html.Style.heightPx 50
                        , Html.Style.aspectRatio "1"
                        , Html.Style.borderRadius "100%"
                        , Html.Style.backgroundColor View.Color.gray100
                        ]
                , [ [ mail.message
                        |> Html.text
                        |> List.singleton
                        |> Html.div
                            [ Html.Style.backgroundColor View.Color.gray100
                            , Html.Style.paddingPx 8
                            , Html.Style.borderRadiusPx 8
                            ]
                    ]
                  , mail.request
                        |> Maybe.map
                            (\item ->
                                [ [ ItemBlock item
                                        |> Data.Block.toString
                                        |> Html.text
                                        |> List.singleton
                                        |> Html.div
                                            [ Html.Style.displayFlex
                                            , Html.Style.justifyContentFlexEnd
                                            , Html.Style.flex "1"
                                            , Html.Style.fontSizePx 40
                                            ]
                                  , if mail.accepted then
                                        Html.text "You will send the item at the end of the day"

                                    else
                                        View.Button.toHtml []
                                            { label = "Send"
                                            , onPress = args.onAccept i
                                            }
                                  ]
                                    |> Html.div
                                        [ Html.Style.displayFlex
                                        , Html.Style.alignItemsCenter
                                        , Html.Style.justifyContentSpaceBetween
                                        ]
                                ]
                            )
                        |> Maybe.withDefault []
                  , mail.present
                        |> Maybe.map
                            (\item ->
                                [ [ ItemBlock item
                                        |> Data.Block.toString
                                        |> Html.text
                                        |> List.singleton
                                        |> Html.div [ Html.Style.fontSizePx 40 ]
                                  , if mail.accepted then
                                        Html.text "You will get the item at the end of the day"

                                    else
                                        View.Button.toHtml []
                                            { label = "Accept"
                                            , onPress = args.onAccept i
                                            }
                                  ]
                                    |> Html.div
                                        [ Html.Style.displayFlex
                                        , Html.Style.alignItemsCenter
                                        , Html.Style.justifyContentSpaceBetween
                                        ]
                                ]
                            )
                        |> Maybe.withDefault []
                  ]
                    |> List.concat
                    |> Html.div
                        [ Html.Style.displayFlex
                        , Html.Style.flexDirectionColumn
                        , Html.Style.gapPx 4
                        ]
                ]
                    |> Html.div
                        [ Html.Style.displayFlex
                        , Html.Style.flexDirectionRow
                        , Html.Style.gapPx 8
                        ]
            )
        |> toHtml
            { show = args.show
            , selected = MailTab
            , onClose = args.onClose
            , onSelectTab = args.onSelectTab
            }


market :
    { show : Bool
    , trades : List Trade
    , onClose : msg
    , onSelectTab : MenuTab -> msg
    , onAcceptTrade : Trade -> msg
    , items : ItemBag
    }
    -> Html msg
market args =
    args.trades
        |> List.map
            (\trade ->
                let
                    cost =
                        trade.remove
                            |> List.concatMap
                                (\( items, n ) ->
                                    items
                                        |> ItemBlock
                                        |> Data.Block.toString
                                        |> List.repeat n
                                )
                            |> String.concat
                in
                [ trade.trader
                    |> Html.text
                    |> List.singleton
                    |> Html.div []
                , trade.add
                    |> ItemBlock
                    |> Data.Block.toString
                    |> Html.text
                    |> List.singleton
                    |> Html.div [ Html.Style.fontSizePx 50 ]
                , if
                    trade.remove
                        |> List.all (\( item, n ) -> Data.ItemBag.contains n item args.items)
                  then
                    View.Button.withIcons [ View.Button.primary ]
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
                        , Html.Style.aspectRatio "1"
                        , Html.Style.widthPx 150
                        , Html.Style.boxSizingBorderBox
                        , Html.Style.borderRadiusPx 8
                        , Html.Style.paddingPx 8
                        ]
            )
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionRow
            , Html.Style.flexWrapWrap
            , Html.Style.gapPx 16
            , Html.Style.height "100%"
            , Html.Style.boxSizingBorderBox
            , Html.Style.justifyContentCenter
            , Html.Style.alignContentCenter
            ]
        |> List.singleton
        |> toHtml
            { show = args.show
            , selected = MarketTab
            , onClose = args.onClose
            , onSelectTab = args.onSelectTab
            }


calender :
    { show : Bool
    , date : Date
    , events : Dict Date Event
    , onClose : msg
    , onSelectTab : MenuTab -> msg
    }
    -> Html msg
calender args =
    [ (if Date.summer args.date then
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
                (if Date.summer args.date then
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
      , Date.listOfDaysInMonth args.date
            |> List.map
                (\date ->
                    Dict.get date args.events
                        |> Maybe.map
                            (\event ->
                                [ View.CalenderDay.eventToString event
                                    |> Maybe.withDefault (Date.day date |> String.fromInt)
                                    |> Html.text
                                    |> List.singleton
                                    |> Html.div
                                        (if date == args.date then
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
                                            ++ (if date == args.date then
                                                    [ Html.Style.backgroundColor View.Color.red900
                                                    , Html.Style.fontWeightBold
                                                    ]

                                                else
                                                    []
                                               )
                                        )
                                , (event.reward
                                    |> Maybe.map (\item -> Data.Block.toString (ItemBlock item))
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

      -- , ( MarketTab, "Market" )
      , ( MailTab, "Messages" )
      ]
        |> List.map
            (\( tab, label ) ->
                if tab == args.selected then
                    View.Button.fake label

                else
                    View.Button.toHtml []
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
    , View.Button.toHtml []
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
