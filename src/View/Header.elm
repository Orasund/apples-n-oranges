module View.Header exposing (..)

import Event exposing (Event)
import Html exposing (Html)
import Html.Style
import View.Button
import View.CalenderDay
import View.DayOfTheWeek


viewHeader :
    { onUndo : msg
    , onOpenCalender : msg
    , currentEvent : Event
    , currentDay : Int
    }
    -> Html msg
viewHeader args =
    Html.div
        [ Html.Style.displayFlex
        , Html.Style.alignItemsEnd
        , Html.Style.justifyContentCenter
        , Html.Style.gapPx 4
        , Html.Style.width "100%"
        ]
        [ View.Button.toHtml
            { label = "Undo"
            , onPress = args.onUndo

            --, price = undoPrice
            }
            |> List.singleton
            |> Html.div [ Html.Style.flex "1", Html.Style.displayFlex ]
        , [ Html.text (View.DayOfTheWeek.toLongString args.currentDay)
          , args.currentEvent
                |> View.CalenderDay.calenderDay 40
                    []
          ]
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                , Html.Style.flexDirectionColumn
                , Html.Style.alignItemsCenter
                ]
        , View.Button.toHtml
            { label = "Calender"
            , onPress = args.onOpenCalender
            }
            |> List.singleton
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                , Html.Style.justifyContentEnd
                ]
        ]
