module View.Calender exposing (..)

import Html exposing (Html)
import Html.Style
import View.Background
import View.Button
import View.DayOfTheWeek


toHtml : { show : Bool, onClose : msg } -> Html msg
toHtml args =
    [ [ List.range 1 7
            |> List.map
                (\n ->
                    View.DayOfTheWeek.toShortString n
                        |> Html.text
                        |> List.singleton
                        |> Html.div
                            [ Html.Style.textAlignCenter
                            , Html.Style.paddingBottomPx 8
                            , Html.Style.borderBottom "1px solid black"
                            ]
                )
      , List.range 1 28
            |> List.map
                (\n ->
                    [ Html.text (String.fromInt n)
                        |> List.singleton
                        |> Html.div []
                    , [ Html.text "A" |> List.singleton |> Html.div []
                      , Html.text "B" |> List.singleton |> Html.div []
                      ]
                        |> Html.div [ Html.Style.displayFlex ]
                    ]
                        |> Html.div
                            [ Html.Style.positionRelative
                            , Html.Style.displayFlex
                            , Html.Style.flexDirectionColumn
                            , Html.Style.alignItemsCenter
                            , Html.Style.justifyContentCenter
                            , Html.Style.paddingPx 4
                            ]
                )
      ]
        |> List.concat
        |> Html.div
            [ Html.Style.displayGrid
            , Html.Style.gridTemplateColumns "repeat(7,1fr)"
            , Html.Style.backgroundColor "white"
            , Html.Style.widthPx 360
            , Html.Style.boxSizingBorderBox
            , Html.Style.paddingPx 8
            , Html.Style.borderRadiusPx 8
            ]
    , View.Button.toHtml
        { label = "Close"
        , onPress = args.onClose
        }
    ]
        |> View.Background.wood
            [ Html.Style.displayFlex
            , Html.Style.alignItemsCenter
            , Html.Style.justifyContentCenter
            , Html.Style.positionAbsolute
            , Html.Style.transition "bottom 1s"
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
