module View.Button exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style
import Layout


primary : Attribute msg
primary =
    Html.Attributes.class "primary-button"


withIcons : List (Attribute msg) -> { label : String, onPress : msg } -> String -> Html msg
withIcons attrs args content =
    [ content
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 20 ]
    , Html.text args.label
    ]
        |> Html.button
            (Layout.asButton
                { onPress = Just args.onPress, label = args.label }
                ++ attrs
                ++ [ Html.Attributes.class "button-base button" ]
            )


toHtml : List (Attribute msg) -> { label : String, onPress : msg } -> Html msg
toHtml attrs args =
    args.label
        |> Html.text
        |> List.singleton
        |> Html.button
            (Layout.asButton
                { onPress = Just args.onPress, label = args.label }
                ++ attrs
                ++ [ Html.Attributes.class "button-base button" ]
            )


fake : String -> Html msg
fake label =
    label
        |> Html.text
        |> List.singleton
        |> Html.div
            [ Html.Attributes.class "button-base"
            , Html.Style.backgroundColor "white"
            , Html.Style.color "black"
            ]
