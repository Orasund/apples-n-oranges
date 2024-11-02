module View.Coin exposing (..)

import Html exposing (Attribute, Html)
import Html.Style


toHtml : List (Attribute msg) -> Int -> Html msg
toHtml attrs n =
    String.fromInt n
        |> Html.text
        |> List.singleton
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.alignItemsCenter
            ]
        |> List.singleton
        |> Html.div
            ([ Html.Style.borderRadius "100%"
             , Html.Style.border "2px solid black"
             , Html.Style.aspectRatio "1"
             , Html.Style.displayFlex
             , Html.Style.justifyContentCenter
             , Html.Style.alignContentCenter
             , Html.Style.boxSizingBorderBox
             , Html.Style.fontWeightBold
             , Html.Style.backgroundColor "#d8d42a"
             ]
                ++ attrs
            )
