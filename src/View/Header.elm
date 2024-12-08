module View.Header exposing (..)

import Event exposing (Event)
import Html exposing (Html)
import Html.Style
import View.Button


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
            }
            |> List.singleton
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                ]

        {--, [ View.Button.toHtml
                { label = "Calender"
                , onPress = args.onOpenCalender
                }
          ]
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                , Html.Style.justifyContentEnd
                ]--}
        ]
