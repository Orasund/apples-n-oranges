module View.Background exposing (..)

import Html exposing (Attribute, Html)
import Html.Style
import View.Color


urlEncode : String -> String
urlEncode =
    String.replace "#" "%23"


toHtml : List (Attribute msg) -> List (Html msg) -> Html msg
toHtml attrs =
    Html.div
        ([ Html.Style.height "100%"
         , Html.Style.width "100%"
         , Html.Style.displayFlex
         , Html.Style.flexDirectionColumn
         , Html.Style.gapPx 16
         ]
            ++ attrs
        )


summerGrass : List (Attribute msg) -> List (Html msg) -> Html msg
summerGrass attrs =
    toHtml
        (Html.Style.backgroundImage
            ("""url('data:image/svg+xml,<svg height="200" width="200" xmlns="http://www.w3.org/2000/svg"><rect width="200" height="200" x="0" y="0" fill="
            """
                ++ urlEncode View.Color.green100
                ++ """
            " /><path d="M0 100 L100 0 L 200 0 L0 200 Z" fill="
            """
                ++ urlEncode View.Color.green200
                ++ """
            " /><path d="M100 200 L200 100 L200 200 Z" fill="
            """
                ++ urlEncode View.Color.green200
                ++ """" /></svg>')"""
                |> String.replace "\n" ""
            )
            :: attrs
        )


endOfDay : List (Attribute msg) -> List (Html msg) -> Html msg
endOfDay attrs =
    toHtml
        (Html.Style.backgroundImage
            ("""url('data:image/svg+xml,<svg height="100" width="100" xmlns="http://www.w3.org/2000/svg"><rect width="100" height="100" x="0" y="0" fill="
            """
                ++ View.Color.blue200
                ++ """
            "/><circle r="10" cx="25" cy="25" fill="
            """
                ++ View.Color.blue100
                ++ """
            "/><circle r="10" cx="75" cy="75" fill="
            """
                ++ View.Color.blue100
                ++ """
            "/></svg>')"""
                |> String.replace "\n" ""
            )
            :: attrs
        )


wood : List (Attribute msg) -> List (Html msg) -> Html msg
wood attrs =
    toHtml
        ([ Html.Style.backgroundImage
            "linear-gradient(0deg, #c6c1ae 50%,#b0a788 50%)"
         , Html.Style.backgroundSize "100px 100px"
         ]
            ++ attrs
        )
