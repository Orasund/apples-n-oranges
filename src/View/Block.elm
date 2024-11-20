module View.Block exposing (..)

import Game exposing (Block)
import Html exposing (Attribute, Html)
import Html.Style
import View.Field


apple : String
apple =
    "🍎"


orange : String
orange =
    "🍊"


lemon : String
lemon =
    "🍋"


pear : String
pear =
    "🍐"


stone : String
stone =
    "🪨"


sprout : String
sprout =
    "🌱"


dynamite : String
dynamite =
    "💣"


grapes : String
grapes =
    "🍇"


toString : Block -> String
toString block =
    case block of
        Game.FruitBlock Game.Apple ->
            apple

        Game.FruitBlock Game.Orange ->
            orange

        Game.FruitBlock Game.Lemon ->
            lemon

        Game.FruitBlock Game.Grapes ->
            grapes

        Game.SolidBlock Game.Stone ->
            stone

        Game.SolidBlock Game.Sprout ->
            sprout

        Game.SolidBlock Game.Dynamite ->
            dynamite


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
