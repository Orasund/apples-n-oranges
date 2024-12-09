module View.Button exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Style
import Layout


withIcons : { label : String, onPress : msg } -> String -> Html msg
withIcons args content =
    [ content
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 20 ]
    , Html.text args.label
    ]
        |> Html.button
            (Layout.asButton
                { onPress = Just args.onPress, label = args.label }
                ++ [ Html.Attributes.class "button-base button" ]
            )


toHtml : { label : String, onPress : msg } -> Html msg
toHtml args =
    args.label
        |> Html.text
        |> List.singleton
        |> Html.button
            (Layout.asButton
                { onPress = Just args.onPress, label = args.label }
                ++ [ Html.Attributes.class "button-base button" ]
            )


fake : { label : String, onPress : msg } -> Html msg
fake args =
    args.label
        |> Html.text
        |> List.singleton
        |> Html.div
            [ Html.Attributes.class "button-base"
            , Html.Style.backgroundColor "white"
            , Html.Style.color "black"
            ]
