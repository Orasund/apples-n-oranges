module View.Block exposing (..)

import Html exposing (Attribute, Html)
import Html.Style


apple : String
apple =
    "🍎"


orange : String
orange =
    "🍊"


stone : String
stone =
    "🪨"


pig : String
pig =
    "🐖"


cow : String
cow =
    "🐄"


sheep : String
sheep =
    "🐑"


chicken : String
chicken =
    "🐓"


rocking : Attribute msg
rocking =
    Html.Style.animation "1s rocking ease-in-out infinite"


small : Attribute msg
small =
    Html.Style.scale "0.5"


shrink : Attribute msg
shrink =
    Html.Style.scale "0"


toHtml : List (Attribute msg) -> String -> Html msg
toHtml attrs string =
    Html.div
        ([ Html.Style.fontSizeRem 4
         , Html.Style.widthMinContent
         , Html.Style.aspectRatio "1"
         , Html.Style.rotate "0deg"
         , Html.Style.positionAbsolute
         , Html.Style.transition "top 0.2s ease-in, left 0.2s ease-in, scale 0.2s ease-in"
         ]
            ++ attrs
        )
        [ Html.text string ]
