module View.Header exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Style
import Layout
import View.Button
import View.Coin


viewHeader : { money : Int, onUndo : msg, onOpenShop : msg } -> Html msg
viewHeader args =
    Html.div
        [ Html.Style.displayFlex
        , Html.Style.alignItemsCenter
        , Html.Style.justifyContentCenter
        , Html.Style.gapPx 4
        , Html.Style.width "100%"
        ]
        [ View.Button.withPrice
            { price = 1
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
        , {--View.Button.toHtml
            { label = "Open Shop"
            , onPress = args.onOpenShop
            }
            |> List.singleton--}
          []
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                , Html.Style.justifyContentEnd
                ]
        ]
