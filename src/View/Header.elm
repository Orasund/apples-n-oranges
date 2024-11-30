module View.Header exposing (..)

import Event exposing (Event)
import Html exposing (Html)
import Html.Style
import View.Button
import View.CalenderDay
import View.EndOfDay


viewHeader :
    { onUndo : msg
    , onOpenShop : msg
    , currentEvent : Event
    , currentDay : Int
    }
    -> Html msg
viewHeader args =
    Html.div
        [ Html.Style.displayFlex
        , Html.Style.alignItemsCenter
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
        , [ Html.text (View.EndOfDay.dayOfTheWeek args.currentDay)
          , args.currentEvent
                |> View.CalenderDay.calenderDay 40
                    []

          {--View.Button.toHtml
            { label = "Open Shop"
            , onPress = args.onOpenShop
            }
            |> List.singleton--}
          --[]
          ]
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                , Html.Style.justifyContentEnd
                , Html.Style.alignItemsCenter
                , Html.Style.gapPx 8
                ]
        ]
