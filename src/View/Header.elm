module View.Header exposing (..)

import Event exposing (Event)
import Html exposing (Html)
import Html.Style
import View.Button
import View.CalenderDay
import View.Coin


undoPrice =
    2


viewHeader :
    { money : Int
    , onUndo : msg
    , onOpenShop : msg
    , currentEvent : Event
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
        [ View.Button.withPrice
            { price = undoPrice
            , label = "Undo"
            , onPress = args.onUndo
            }
            |> List.singleton
            |> Html.div [ Html.Style.flex "1", Html.Style.displayFlex ]
        , View.Coin.toHtml
            [ Html.Style.fontSizePx 48
            , Html.Style.heightPx 100
            , Html.Style.borderWidthPx 8
            ]
            (args.money |> min 999 |> max -99)
            |> List.singleton
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                , Html.Style.justifyContentCenter
                ]
        , args.currentEvent
            |> View.CalenderDay.calenderDay 40
                []
            {--View.Button.toHtml
            { label = "Open Shop"
            , onPress = args.onOpenShop
            }
            |> List.singleton--}
            --[]
            |> List.singleton
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                , Html.Style.justifyContentEnd
                ]
        ]
