module View.EndOfDay exposing (..)

import Event exposing (Event(..))
import Html exposing (Html)
import Html.Keyed
import Html.Style
import View.Background
import View.CalenderDay


calenderSize =
    300


title : Int -> String
title n =
    case modBy 7 n of
        1 ->
            "Monday"

        2 ->
            "Tuesday"

        3 ->
            "Wednesday"

        4 ->
            "Thursday"

        5 ->
            "Friday"

        6 ->
            "Saturday"

        0 ->
            "Sunday"

        _ ->
            "Day " ++ String.fromInt n


toHtml :
    { money : Int
    , currentEvent : Event
    , nextEvents : List Event
    , endOfDay : Bool
    , day : Int
    }
    -> Html msg
toHtml args =
    [ Html.div [ Html.Style.fontSizePx 75 ] [ Html.text (title args.day) ]
    , ( args.day, args.currentEvent )
        :: List.indexedMap (\i -> Tuple.pair (i + 1 + args.day)) args.nextEvents
        |> List.map
            (\( i, event ) ->
                ( String.fromInt i
                , View.CalenderDay.calenderDay calenderSize
                    [ Html.Style.positionAbsolute
                    , Html.Style.bottomPx (0 - toFloat (i - args.day) * (calenderSize + toFloat 64))
                    , Html.Style.transition "bottom 1s"
                    ]
                    event
                )
            )
        |> List.sortBy Tuple.first
        |> Html.Keyed.node "div"
            [ Html.Style.displayFlex
            , Html.Style.widthPx calenderSize
            , Html.Style.heightPx calenderSize
            , Html.Style.overflowHidden
            , Html.Style.positionRelative
            ]
    ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.alignItemsCenter
            , Html.Style.gapPx 32
            ]
        |> List.singleton
        |> View.Background.endOfDay
            [ Html.Style.positionAbsolute
            , Html.Style.height "100vh"
            , Html.Style.width "100%"
            , Html.Style.transition "bottom 1s"
            , Html.Style.bottom
                (if args.endOfDay then
                    "0vh"

                 else
                    "100vh"
                )
            , Html.Style.displayFlex
            , Html.Style.justifyContentCenter
            , Html.Style.alignItemsCenter
            , Html.Style.overflowHidden
            ]
