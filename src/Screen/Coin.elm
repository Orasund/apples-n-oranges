module Screen.Coin exposing (..)

import Data.Block exposing (Block(..), Optional(..))
import Html exposing (Html)
import Html.Style
import View.Background
import View.Button


toHtml : { onClose : msg } -> Html msg
toHtml args =
    [ [ Html.text "You get a coin."
      , OptionalBlock Coin
            |> Data.Block.toString
            |> Html.text
            |> List.singleton
            |> Html.div [ Html.Style.fontSizePx 50 ]
      , View.Button.toHtml
            { label = "Accept"
            , onPress = args.onClose
            }
      ]
        |> Html.div
            [ Html.Style.backgroundColor "#fdfff1"
            , Html.Style.paddingPx 16
            , Html.Style.borderRadiusPx 8
            , Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.alignItemsCenter
            , Html.Style.gapPx 16
            , Html.Style.widthPx 350
            , Html.Style.boxSizingBorderBox
            ]
    ]
        |> View.Background.wood
            [ Html.Style.displayFlex
            , Html.Style.alignItemsCenter
            , Html.Style.justifyContentCenter
            , Html.Style.positionAbsolute
            , Html.Style.gapPx 32
            , Html.Style.width "100%"
            , Html.Style.height "100%"
            ]
