module View.TitleScreen exposing (..)

import Html exposing (Attribute, Html)
import Html.Style
import Layout
import View.Background


fruit : List (Attribute msg) -> { id : Int, onSelect : Int -> msg } -> Html msg -> Html msg
fruit attrs args content =
    content
        |> List.singleton
        |> Html.div
            (Layout.asButton
                { label = "select"
                , onPress = Just (args.onSelect args.id)
                }
                ++ [ Html.Style.positionAbsolute
                   , Html.Style.fontSizePx 50
                   ]
                ++ attrs
            )


toHtml : { show : Bool, onSelect : Int -> msg } -> Html msg
toHtml args =
    [ [ [ Html.text "Apples"
        , Html.text "🍎"
            |> fruit
                [ Html.Style.bottomPx -30
                , Html.Style.leftPx 20
                ]
                { id = 0, onSelect = args.onSelect }
        , Html.text "🍎"
            |> fruit
                [ Html.Style.bottomPx -10
                , Html.Style.rightPx 30
                ]
                { id = 1, onSelect = args.onSelect }
        ]
            |> Html.div [ Html.Style.positionRelative ]
      , Html.text "&"
            |> List.singleton
            |> Html.div [ Html.Style.positionRelative ]
      , [ Html.text "Oranges"
        , Html.text "🍊"
            |> fruit
                [ Html.Style.bottomPx -30
                , Html.Style.rightPx 30
                ]
                { id = 2, onSelect = args.onSelect }
        , Html.text "🍊"
            |> List.singleton
            |> Html.div
                [ Html.Style.positionAbsolute
                , Html.Style.bottomPx -10
                , Html.Style.leftPx 10
                , Html.Style.fontSizePx 50
                ]
        ]
            |> Html.div [ Html.Style.positionRelative ]
      ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.alignItemsCenter
            , Html.Style.fontSizePx 75
            ]
    , Html.text "Click on two different fruits to start"
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 24 ]
    ]
        |> View.Background.game
            [ Html.Style.displayFlex
            , Html.Style.alignItemsCenter
            , Html.Style.justifyContentCenter
            , Html.Style.positionAbsolute
            , Html.Style.bottom
                (if args.show then
                    "0vh"

                 else
                    "100vh"
                )
            , Html.Style.gapPx 64
            , Html.Style.width "100%"
            , Html.Style.height "100%"
            ]
