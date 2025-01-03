module View.Button exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style
import Layout
import View.Color


primary : Attribute msg
primary =
    Html.Attributes.class "primary"


chip : Attribute msg
chip =
    Html.Attributes.class "chip"


active : Attribute msg
active =
    Html.Attributes.class "active"


asIcon : List (Attribute msg)
asIcon =
    [ Html.Style.aspectRatio "1"
    , Html.Style.paddingPx 0
    , Html.Style.justifyContentCenter
    ]


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
        [ Html.Style.heightPx 24
        , Html.Style.aspectRatio "1"
        , Html.Style.borderRadiusPx 12
        , Html.Style.backgroundColor View.Color.red900
        , Html.Style.positionAbsolute
        , Html.Style.rightPx -6
        , Html.Style.topPx -6
        , Html.Style.fontSizePx 14
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


withIconAndContent : List (Attribute msg) -> { label : String, onPress : msg, icon : String } -> Html msg -> Html msg
withIconAndContent attrs args content =
    [ args.icon
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 20 ]
    , Html.text args.label
    , content
    ]
        |> Html.button
            (Layout.asButton
                { onPress = Just args.onPress, label = args.label }
                ++ attrs
                ++ [ Html.Attributes.class "button-base button"
                   , Html.Style.positionRelative
                   ]
            )


withContent : List (Attribute msg) -> { label : String, onPress : msg } -> Html msg -> Html msg
withContent attrs args content =
    [ Html.text args.label
    , content
    ]
        |> Html.button
            (Layout.asButton
                { onPress = Just args.onPress, label = args.label }
                ++ attrs
                ++ [ Html.Attributes.class "button-base button"
                   , Html.Style.positionRelative
                   ]
            )
