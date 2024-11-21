module View.EndOfDay exposing (..)

import Html exposing (Attribute, Html)
import Html.Keyed
import Html.Style
import Level.Generator exposing (Setting)
import View.Background
import View.Block
import View.CalenderDay


calenderSize =
    300


viewShop :
    { money : Int
    , currentLevel : Setting
    , nextLevels : List Setting
    , endOfDay : Bool
    , day : Int
    }
    -> Html msg
viewShop args =
    [ ( args.day, args.currentLevel )
        :: List.indexedMap (\i -> Tuple.pair (i + 1 + args.day)) args.nextLevels
        |> List.map
            (\( i, setting ) ->
                ( String.fromInt i
                , View.CalenderDay.calenderDay calenderSize
                    [ Html.Style.positionAbsolute
                    , Html.Style.leftPx -(calenderSize / 2)
                    , Html.Style.bottomPx (-calenderSize / 2 - toFloat (i - args.day) * (calenderSize + toFloat 64))
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
            , Html.Style.top "50vh"
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
                (if args.endOfDay then
                    "0vh"

                 else
                    "100vh"
                )
            , Html.Style.displayFlex

            -- , Html.Style.justifyContentCenter
            , Html.Style.alignItemsCenter
            , Html.Style.overflowHidden
            ]
