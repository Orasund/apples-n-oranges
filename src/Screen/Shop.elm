module Screen.Shop exposing (..)

import Data.Block exposing (Block(..), Optional(..), Organic(..))
import Event exposing (Event(..))
import Html exposing (Html)
import Html.Style
import Puzzle.Builder exposing (Group(..))
import View.Background
import View.Button
import View.Group


calenderSize =
    60


gap =
    16


columns =
    3


toHtml :
    { onBuy : Group -> msg
    , onClose : msg
    }
    -> Html msg
toHtml args =
    [ [ Html.text "Shop" |> List.singleton |> Html.div [ Html.Style.fontSizePx 20 ]
      , [ Pair (OptionalBlock Rock) Pickaxe
        , Pair (OptionalBlock Fish) FishingRod
        ]
            |> List.map
                (\group ->
                    [ View.Group.toHtml group
                    , View.Button.toHtml
                        { label = "Buy"
                        , onPress = args.onBuy group
                        }
                    ]
                        |> Html.div
                            [ Html.Style.displayFlex
                            , Html.Style.flexDirectionColumn
                            , Html.Style.gapPx 8
                            , Html.Style.alignItemsCenter
                            ]
                )
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.gapPx 16
                ]
      , View.Button.toHtml
            { label = "Close"
            , onPress = args.onClose
            }
      ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.gapPx 32
            , Html.Style.widthPx 360
            , Html.Style.heightPx 360
            , Html.Style.alignItemsCenter
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
