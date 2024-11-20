module View.Shop exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (hidden)
import Html.Keyed
import Html.Style
import Layout
import Level.Generator exposing (Setting)
import View.Block
import View.Coin


calenderDay : List (Attribute msg) -> Setting -> Html msg
calenderDay attrs setting =
    Html.div
        ([ Html.Style.displayFlex
         , Html.Style.flexDirectionColumn
         , Html.Style.fontSizePx 20
         , Html.Style.backgroundColor "#fff"
         , Html.Style.borderRadiusPx 8
         , Html.Style.overflowHidden
         , Html.Style.heightPx 50
         , Html.Style.widthPx 50
         ]
            ++ attrs
        )
        [ Html.div
            [ Html.Style.backgroundColor "red"
            , Html.Style.heightPx 10
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
    , onGotoLevel : msg
    , openShop : Bool
    , day : Int
    }
    -> Html msg
viewShop args =
    [ View.Coin.toHtml
        [ Html.Style.fontSizePx 48
        , Html.Style.heightPx 100
        , Html.Style.borderWidthPx 8
        ]
        (args.money |> min 999 |> max -99)
        |> List.singleton
        |> Html.div
            [ Html.Style.flex "1"
            , Html.Style.displayFlex
            , Html.Style.justifyContentCenter
            ]
    , ( args.day, args.currentLevel )
        :: List.indexedMap (\i -> Tuple.pair (i + 1 + args.day)) args.nextLevels
        |> List.map
            (\( i, setting ) ->
                ( String.fromInt i
                , calenderDay
                    [ Html.Style.positionAbsolute
                    , Html.Style.leftPx (140 - 25 + (i - args.day) * (50 + 8) |> toFloat)
                    , Html.Style.transition "left 1s"
                    ]
                    setting
                )
            )
        |> List.sortBy Tuple.first
        |> Html.Keyed.node "div"
            [ Html.Style.displayFlex
            , Html.Style.gapPx 8
            , Html.Style.heightPx 50
            , Html.Style.widthPx 280
            , Html.Style.leftPx 60
            , Html.Style.overflowHidden
            , Html.Style.positionRelative
            ]
    , "Next Level"
        |> Html.text
        |> List.singleton
        |> Html.button
            (Layout.asButton
                { onPress = Just args.onGotoLevel
                , label = "Next Level"
                }
                ++ [ Html.Attributes.class "button"
                   , Html.Style.padding "8px 4px"
                   , Html.Style.displayFlex
                   , Html.Style.alignItemsCenter
                   , Html.Style.justifyContentCenter
                   ]
            )
    ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.gapPx 16
            , Html.Style.widthPx 400
            ]
        |> List.singleton
        |> Html.div
            [ Html.Style.positionAbsolute
            , Html.Style.backgroundImage
                "linear-gradient(0deg, #998c5e 50%,#a79d7a 50%)"
            , Html.Style.backgroundSize "100px 100px"
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
            ]
