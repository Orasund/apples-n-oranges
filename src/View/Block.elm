module View.Block exposing (..)

import Html exposing (Attribute, Html)
import Html.Style
import View.Field


rocking : Attribute msg
rocking =
    Html.Style.animation "1s rocking ease-in-out infinite"


small : Attribute msg
small =
    Html.Style.scale "0.5"


shrink : Attribute msg
shrink =
    Html.Style.scale "0"


withContent : List (Attribute msg) -> List (Html msg) -> Html msg
withContent attrs =
    Html.div
        ([ Html.Style.fontSizePx (View.Field.size * 0.8)
         , Html.Style.widthMinContent
         , Html.Style.aspectRatio "1"
         , Html.Style.rotate "0deg"
         , Html.Style.positionAbsolute
         , Html.Style.transition "top 0.2s ease-in, left 0.2s ease-in, scale 0.2s ease-in"
         , Html.Style.displayFlex
         , Html.Style.justifyContentCenter
         , Html.Style.alignItemsCenter
         ]
            ++ attrs
        )
