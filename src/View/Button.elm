module View.Button exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style
import Layout
import View.Color


primary : Attribute msg
primary =
    Html.Attributes.class "primary-button"


big : List (Attribute msg)
big =
    [ Html.Style.fontSizePx 28
    , Html.Style.widthPx 100
    , Html.Style.heightPx 100
    , Html.Style.borderRadius "100%"
    , Html.Style.displayFlex
    , Html.Style.alignItemsCenter
    , Html.Style.justifyContentCenter
    ]


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


withDot : List (Attribute msg) -> { label : String, onPress : msg, amount : Int } -> Html msg
withDot attrs args =
    [ Html.div
        [ Html.Style.heightPx 16
        , Html.Style.aspectRatio "1"
        , Html.Style.borderRadiusPx 8
        , Html.Style.backgroundColor View.Color.red900
        , Html.Style.positionAbsolute
        , Html.Style.rightPx -4
        , Html.Style.topPx -4
        , Html.Style.fontSizePx 10
        , Html.Style.color View.Color.white
        , Html.Style.boxSizingBorderBox
        , Html.Style.displayFlex
        , Html.Style.justifyContentCenter
        , Html.Style.alignItemsCenter
        ]
        [ Html.text (String.fromInt args.amount) ]
    , Html.text args.label
    ]
        |> Html.button
            (Layout.asButton
                { onPress = Just args.onPress, label = args.label }
                ++ attrs
                ++ [ Html.Attributes.class "button-base button"
                   , Html.Style.positionRelative
                   ]
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
