module View.Person exposing (..)

import Data.Person exposing (Person)
import Html exposing (Attribute, Html)
import Html.Style
import View.Color


big : Attribute msg
big =
    Html.Style.fontSizePx 120


toHtml : List (Attribute msg) -> Person -> Html msg
toHtml attrs person =
    [ (if person.friendship >= Data.Person.friendshipForLove then
        "❤️"

       else if person.friendship >= Data.Person.friendshipForLove // 2 then
        "🧡"

       else
        ""
      )
        |> Html.text
        |> List.singleton
        |> Html.div
            [ Html.Style.positionAbsolute
            , Html.Style.bottom "-0.75em"
            , Html.Style.fontSize "0.5em"
            , Html.Style.textShadow "0 0 1px black"
            ]
    , person.symbol
        |> Html.text
        |> List.singleton
        |> Html.div
            ([ Html.Style.displayFlex
             , Html.Style.alignItemsCenter
             , Html.Style.justifyContentCenter
             , Html.Style.width "100%"
             , Html.Style.aspectRatio "1"
             , Html.Style.borderRadius "100%"
             ]
                ++ (if person.friendship >= Data.Person.friendshipForLove then
                        [ Html.Style.backgroundColor View.Color.red100
                        , Html.Style.border ("1em solid " ++ View.Color.red900)
                        , Html.Style.boxSizingBorderBox
                        ]

                    else if person.friendship >= Data.Person.friendshipForLove // 2 then
                        [ Html.Style.backgroundColor View.Color.yellow100
                        , Html.Style.border ("0.5em solid " ++ View.Color.yellow900)
                        , Html.Style.boxSizingBorderBox
                        ]

                    else
                        [ Html.Style.backgroundColor View.Color.gray100
                        ]
                   )
            )
    ]
        |> Html.div
            ([ Html.Style.displayFlex
             , Html.Style.flexDirectionColumn
             , Html.Style.alignItemsCenter
             , Html.Style.justifyContentStart
             , Html.Style.positionRelative
             , Html.Style.fontSizePx 20
             , Html.Style.width "1.5em"
             ]
                ++ attrs
            )
