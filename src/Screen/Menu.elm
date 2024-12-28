module Screen.Menu exposing (..)

import Data.Block exposing (Block(..), Item(..))
import Data.Date as Date exposing (Date)
import Data.ItemBag exposing (ItemBag)
import Data.Person exposing (Message, Person)
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
    }


personBubble : Person -> Html msg
personBubble person =
    person.symbol
        |> Html.text
        |> List.singleton
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.alignItemsCenter
            , Html.Style.justifyContentCenter
            , Html.Style.heightPx 35
            , Html.Style.aspectRatio "1"
            , Html.Style.borderRadius "100%"
            , Html.Style.backgroundColor View.Color.gray100
            , Html.Style.fontSizePx 20
            ]
        |> List.singleton
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.alignItemsCenter
            , Html.Style.justifyContentStart
            ]


attachment : { onAccept : msg, items : ItemBag, answered : Bool } -> Message -> Maybe (Html msg)
attachment args mail =
    mail.present
        |> Maybe.map
            (\item ->
                [ Data.Block.toString (ItemBlock item)
                    |> Html.text
                    |> List.singleton
                    |> Html.div
                        [ Html.Style.fontSizePx
                            (if args.answered then
                                20

                             else
                                40
                            )
                        ]
                , if args.answered then
                    Html.text "Accepted"
                        |> List.singleton
                        |> Html.div []

                  else if Data.ItemBag.size args.items >= Data.ItemBag.maxAmountOfItems then
                    Html.text "You don't have enough space"
                        |> List.singleton
                        |> Html.div []

                  else
                    View.Button.toHtml
                        [ View.Button.primary
                        ]
                        { label = "Accept"
                        , onPress = args.onAccept
                        }
                ]
                    |> Html.div
                        ([ Html.Style.displayFlex
                         , Html.Style.alignItemsCenter
                         , Html.Style.justifyContentSpaceBetween
                         , Html.Style.borderRadiusPx 8
                         , Html.Style.paddingPx 8
                         ]
                            ++ (if args.answered then
                                    [ Html.Style.backgroundColor View.Color.white
                                    , Html.Style.borderWidthPx 1
                                    , Html.Style.borderStyleSolid
                                    , Html.Style.borderColor View.Color.gray100
                                    ]

                                else
                                    [ Html.Style.backgroundColor View.Color.green100
                                    ]
                               )
                        )
            )


request : { onAccept : msg, items : ItemBag, answered : Bool } -> Message -> Maybe (Html msg)
request args mail =
    mail.request
        |> Maybe.map
            (\item ->
                [ Data.Block.toString (ItemBlock item)
                    |> Html.text
                    |> List.singleton
                    |> Html.div
                        [ Html.Style.fontSizePx
                            (if args.answered then
                                20

                             else
                                40
                            )
                        ]
                , if args.answered then
                    Html.text "Sent"
                        |> List.singleton
                        |> Html.div []

                  else if Data.ItemBag.contains 1 item args.items |> not then
                    Html.text "You dont have the item"
                        |> List.singleton
                        |> Html.div []

                  else
                    View.Button.toHtml
                        [ View.Button.primary
                        ]
                        { label = "Send"
                        , onPress = args.onAccept
                        }
                ]
                    |> Html.div
                        ([ Html.Style.displayFlex
                         , Html.Style.alignItemsCenter
                         , Html.Style.justifyContentSpaceBetween
                         , Html.Style.borderRadiusPx 8
                         , Html.Style.paddingPx 8
                         , Html.Style.alignSelfEnd
                         , Html.Style.width "80%"
                         ]
                            ++ (if args.answered then
                                    [ Html.Style.backgroundColor View.Color.white
                                    , Html.Style.borderWidthPx 1
                                    , Html.Style.borderStyleSolid
                                    , Html.Style.borderColor View.Color.gray100
                                    ]

                                else
                                    [ Html.Style.backgroundColor View.Color.red100 ]
                               )
                        )
            )


messages :
    { onAccept : Date -> msg
    , items : ItemBag
    }
    -> List { date : Date, person : Person, mail : Message, answered : Bool }
    -> List (Html msg)
messages args list =
    let
        viewMessage { date, person, mail, answered } =
            [ personBubble person
            , [ [ [ [ person.name
                        |> Html.text
                        |> List.singleton
                        |> Html.div [ Html.Style.fontWeightBold ]
                    , mail.content
                        |> Html.text
                        |> List.singleton
                        |> Html.div []
                    ]
                  , attachment
                        { onAccept = args.onAccept date
                        , items = args.items
                        , answered = answered
                        }
                        mail
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                  ]
                    |> List.concat
                    |> Html.div
                        [ Html.Style.borderColor View.Color.gray100
                        , Html.Style.borderStyleSolid
                        , Html.Style.borderWidthPx 1
                        , Html.Style.paddingPx 8
                        , Html.Style.borderRadiusPx 8
                        , Html.Style.displayFlex
                        , Html.Style.flexDirectionColumn
                        , Html.Style.gapPx 8
                        , Html.Style.width "80%"
                        ]
                ]
              , request
                    { onAccept = args.onAccept date
                    , items = args.items
                    , answered = answered
                    }
                    mail
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
              ]
                |> List.concat
                |> Html.div
                    [ Html.Style.displayFlex
                    , Html.Style.flexDirectionColumn
                    , Html.Style.gapPx 4
                    , Html.Style.width "100%"
                    ]
            ]
                |> Html.div
                    [ Html.Style.displayFlex
                    , Html.Style.flexDirectionRow
                    , Html.Style.gapPx 8
                    , if answered then
                        Html.Style.backgroundColor View.Color.white

                      else
                        Html.Style.backgroundColor View.Color.white
                    ]
    in
    list
        |> List.reverse
        |> List.map viewMessage


market :
    { trades : List Trade
    , onAcceptTrade : Trade -> msg
    , items : ItemBag
    }
    -> List (Html msg)
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
                [ {--[ personBubble trade.trader
                  , trade.trader.name
                        |> Html.text
                  ]
                    |> Html.div
                        [ Html.Style.displayFlex
                        , Html.Style.alignItemsCenter
                        , Html.Style.gapPx 8
                        ]
                ,--}
                  trade.add
                    |> ItemBlock
                    |> Data.Block.toString
                    |> Html.text
                    |> List.singleton
                    |> Html.div [ Html.Style.fontSizePx 75 ]
                , "for "
                    ++ cost
                    |> Html.text
                    |> List.singleton
                    |> Html.div []
                , if
                    trade.remove
                        |> List.all (\( item, n ) -> Data.ItemBag.contains n item args.items)
                  then
                    View.Button.toHtml [ View.Button.primary ]
                        { label = "Trade"
                        , onPress = args.onAcceptTrade trade
                        }

                  else
                    Html.div []
                        [ Html.text "You don't have the items" ]
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
                        , Html.Style.borderWidthPx 1
                        , Html.Style.borderStyleSolid
                        , Html.Style.borderColor View.Color.gray100
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


calender :
    { date : Date
    , events : Dict Date Event
    }
    -> List (Html msg)
calender args =
    [ [ (if Date.summer args.date then
            "Summer"

         else
            "Winter"
        )
            |> Html.text
            |> List.singleton
            |> Html.div [ Html.Style.fontSizePx 32 ]
      , "Year "
            ++ String.fromInt (Date.year args.date + 1)
            |> Html.text
            |> List.singleton
            |> Html.div []
      ]
        |> Html.div
            [ Html.Style.height "100%"
            , Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
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
                                            , View.Block.monocolor

                                            --, View.Block.white
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
                                         , Html.Style.fontSizePx 18
                                         ]
                                            ++ (if date == args.date then
                                                    [ Html.Style.backgroundColor View.Color.red900
                                                    , Html.Style.fontWeightBold
                                                    ]

                                                else
                                                    []
                                               )
                                        )

                                {--, (if event.mail then
                                    Html.div
                                        [ Html.Style.widthPx 10
                                        , Html.Style.aspectRatio "1"
                                        , Html.Style.borderRadiusPx 5
                                        , Html.Style.backgroundColor View.Color.blue300
                                        ]
                                        []

                                   else if event.reward then
                                    Html.div
                                        [ Html.Style.widthPx 10
                                        , Html.Style.aspectRatio "1"
                                        , Html.Style.borderRadiusPx 5
                                        , Html.Style.backgroundColor View.Color.yellow300
                                        ]
                                        []

                                   else
                                    Html.text ""
                                  )
                                    |> List.singleton
                                    |> Html.div [ Html.Style.fontSizePx 8 ]--}
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


toHtml :
    { show : Bool
    , onClose : msg
    , selected : Maybe MenuTab
    , onSelectTab : MenuTab -> msg
    }
    -> List (Html msg)
    -> Html msg
toHtml args content =
    [ [ ( CalenderTab, "Calender" )
      , ( MarketTab, "Market" )
      , ( MailTab, "Messages" )
      ]
        |> List.map
            (\( tab, label ) ->
                if Just tab == args.selected then
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
            , Html.Style.overflowYScroll
            ]
    , View.Button.toHtml (View.Button.big ++ [ View.Button.primary ])
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
