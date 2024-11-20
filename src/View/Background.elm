module View.Background exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style


toHtml : List (Attribute msg) -> List (Html msg) -> Html msg
toHtml attrs =
    Html.div
        ([ Html.Style.height "100%"
         , Html.Style.width "100%"
         , Html.Style.displayFlex
         , Html.Style.flexDirectionColumn
         ]
            ++ attrs
        )


game : List (Attribute msg) -> List (Html msg) -> Html msg
game attrs =
    toHtml
        ([ Html.Style.backgroundColor "#a4cc95"
         ]
            ++ attrs
        )


endOfDay : List (Attribute msg) -> List (Html msg) -> Html msg
endOfDay attrs =
    toHtml
        ([ Html.Style.backgroundImage
            """url(endOfDay.svg)"""
         ]
            ++ attrs
        )


shop : List (Attribute msg) -> List (Html msg) -> Html msg
shop attrs =
    toHtml
        ([ Html.Style.backgroundImage
            "linear-gradient(0deg, #998c5e 50%,#a79d7a 50%)"
         , Html.Style.backgroundSize "100px 100px"
         ]
            ++ attrs
        )
