module View.Button exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Style
import Layout
import View.Coin


withPrice : { price : Int, label : String, onPress : msg } -> Html msg
withPrice args =
    [ View.Coin.toHtml
        [ Html.Style.fontSizePx 14
        , Html.Style.heightPx 24
        , Html.Style.borderWidthPx 3
        ]
        args.price
    , Html.div
        [ Html.Style.padding "4px 8px"
        , Html.Style.fontSizePx 16
        ]
        [ Html.text args.label ]
    ]
        |> Html.button
            (Layout.asButton
                { onPress = Just args.onPress, label = args.label }
                ++ [ Html.Attributes.class "button"
                   , Html.Style.paddingPx 2
                   , Html.Style.displayFlex
                   , Html.Style.alignItemsCenter
                   ]
            )


toHtml : { label : String, onPress : msg } -> Html msg
toHtml args =
    args.label
        |> Html.text
        |> List.singleton
        |> Html.button
            (Layout.asButton
                { onPress = Just args.onPress, label = args.label }
                ++ [ Html.Attributes.class "button"
                   , Html.Style.padding "6px 12px"
                   , Html.Style.fontSizePx 16
                   , Html.Style.displayFlex
                   , Html.Style.alignItemsCenter
                   ]
            )
