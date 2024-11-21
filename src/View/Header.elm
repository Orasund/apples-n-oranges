module View.Header exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Style
import Layout
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
        [ [ View.Coin.toHtml
                [ Html.Style.fontSizePx 14
                , Html.Style.heightPx 26
                , Html.Style.borderWidthPx 3
                ]
                1
          , Html.div [ Html.Style.padding "4px 8px" ]
                [ Html.text "Undo" ]
          ]
            |> Html.button
                (Layout.asButton
                    { onPress = Just args.onUndo, label = "Undo" }
                    ++ [ Html.Attributes.class "button"
                       , Html.Style.paddingPx 2
                       , Html.Style.displayFlex
                       , Html.Style.alignItemsCenter
                       ]
                )
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
        , [ View.Coin.toHtml
                [ Html.Style.fontSizePx 14
                , Html.Style.heightPx 26
                , Html.Style.borderWidthPx 3
                ]
                1
          , Html.div [ Html.Style.padding "4px 8px" ]
                [ Html.text "Open Shop" ]
          ]
            |> Html.button
                (Layout.asButton
                    { onPress = Just args.onOpenShop, label = "Undo" }
                    ++ [ Html.Attributes.class "button"
                       , Html.Style.paddingPx 2
                       , Html.Style.displayFlex
                       , Html.Style.alignItemsCenter
                       ]
                )
            |> List.singleton
            |> Html.div [ Html.Style.flex "1", Html.Style.displayFlex ]
        ]
