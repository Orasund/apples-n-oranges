module View.Header exposing (..)

import Html exposing (Html)
import Html.Style
import Puzzle.Setting exposing (Event)
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
        [ View.Button.toHtml [ View.Button.primary ]
            { label = "Undo"
            , onPress = args.onUndo
            }
            |> List.singleton
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                ]
        ]
