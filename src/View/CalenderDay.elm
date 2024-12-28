module View.CalenderDay exposing (..)

import Data.Block exposing (Block(..))
import Html exposing (Attribute, Html)
import Html.Style
import Puzzle.Setting exposing (Event)
import View.Block
import View.Color


eventToString : Event -> Maybe String
eventToString event =
    if event.mail then
        Just "✉️"

    else if event.reward then
        ItemBlock event.setting.reward
            |> Data.Block.toString
            |> Just

    else
        event.setting.symbol
            |> Maybe.map Data.Block.toString


calenderDay : { size : Float, day : Int } -> List (Attribute msg) -> Event -> Html msg
calenderDay args attrs event =
    Html.div
        ([ Html.Style.displayFlex
         , Html.Style.flexDirectionColumn
         , Html.Style.fontSizePx (args.size * 0.5)
         , Html.Style.backgroundColor "#fff"
         , Html.Style.borderRadiusPx (args.size * 0.1)
         , Html.Style.overflowHidden
         , Html.Style.heightPx args.size
         , Html.Style.widthPx args.size
         ]
            ++ attrs
        )
        [ "⭐"
            |> List.repeat event.setting.difficulty
            |> String.join " "
            |> Html.text
            |> List.singleton
            |> Html.span [ View.Block.white ]
            |> List.singleton
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.justifyContentCenter
                , Html.Style.alignItemsCenter
                , Html.Style.backgroundColor View.Color.red900
                , Html.Style.heightPx (args.size / 4)
                , Html.Style.fontSizePx (args.size * 0.1)
                ]
        , eventToString event
            |> Maybe.withDefault (args.day |> String.fromInt)
            |> Html.text
            |> List.singleton
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.justifyContentCenter
                , Html.Style.alignItemsCenter
                , Html.Style.height "100%"
                , Html.Style.boxSizingBorderBox
                , Html.Style.borderBottomLeftRadiusPx (args.size * 0.1)
                , Html.Style.borderBottomRightRadiusPx (args.size * 0.1)
                ]
        ]
