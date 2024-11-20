module View.Shop exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (hidden)
import Html.Keyed
import Html.Style
import Layout
import Level.Generator exposing (Setting)
import View.Background
import View.Block
import View.Coin


calenderSize : Float
calenderSize =
    150


calenderDay : List (Attribute msg) -> Setting -> Html msg
calenderDay attrs setting =
    Html.div
        ([ Html.Style.displayFlex
         , Html.Style.flexDirectionColumn
         , Html.Style.fontSizePx (calenderSize * 0.5)
         , Html.Style.backgroundColor "#fff"
         , Html.Style.borderRadiusPx 16
         , Html.Style.overflowHidden
         , Html.Style.heightPx calenderSize
         , Html.Style.widthPx calenderSize
         ]
            ++ attrs
        )
        [ Html.div
            [ Html.Style.backgroundColor "red"
            , Html.Style.heightPx (calenderSize / 5)
            ]
            []
        , setting.symbol
            |> View.Block.toString
            |> Html.text
            |> List.singleton
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.justifyContentCenter
                , Html.Style.alignItemsCenter
                , Html.Style.height "100%"
                ]
        ]


viewShop :
    { money : Int
    , currentLevel : Setting
    , nextLevels : List Setting
    , openShop : Bool
    , day : Int
    }
    -> Html msg
viewShop args =
    [ ( args.day, args.currentLevel )
        :: List.indexedMap (\i -> Tuple.pair (i + 1 + args.day)) args.nextLevels
        |> List.map
            (\( i, setting ) ->
                ( String.fromInt i
                , calenderDay
                    [ Html.Style.positionAbsolute
                    , Html.Style.leftPx -(calenderSize / 2)
                    , Html.Style.bottomPx (0 - toFloat (i - args.day) * (calenderSize + toFloat 32))
                    , Html.Style.transition "bottom 1s"
                    ]
                    setting
                )
            )
        |> List.sortBy Tuple.first
        |> Html.Keyed.node "div"
            [ Html.Style.displayFlex
            , Html.Style.gapPx 8

            --, Html.Style.heightPx 50
            --, Html.Style.widthPx 280
            --, Html.Style.leftPx 60
            , Html.Style.positionRelative
            ]
    ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.gapPx 16

            --, Html.Style.widthPx 400
            ]
        |> List.singleton
        |> View.Background.endOfDay
            [ Html.Style.positionAbsolute
            , Html.Style.height "100vh"
            , Html.Style.width "100%"
            , Html.Style.transition "bottom 1s"
            , Html.Style.bottom
                (if args.openShop then
                    "0vh"

                 else
                    "100vh"
                )
            , Html.Style.displayFlex
            , Html.Style.justifyContentCenter
            , Html.Style.alignItemsCenter
            , Html.Style.overflowHidden
            ]
