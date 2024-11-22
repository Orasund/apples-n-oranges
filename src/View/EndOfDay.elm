module View.EndOfDay exposing (..)

import Html exposing (Html)
import Html.Keyed
import Html.Style
import Level.Generator exposing (Setting)
import View.Background
import View.CalenderDay


calenderSize =
    300


toHtml :
    { money : Int
    , currentLevel : Setting
    , nextLevels : List Setting
    , endOfDay : Bool
    , day : Int
    }
    -> Html msg
toHtml args =
    [ Html.div [ Html.Style.fontSizePx 100 ] [ Html.text ("Day " ++ String.fromInt args.day) ]
    , ( args.day, args.currentLevel )
        :: List.indexedMap (\i -> Tuple.pair (i + 1 + args.day)) args.nextLevels
        |> List.map
            (\( i, setting ) ->
                ( String.fromInt i
                , View.CalenderDay.calenderDay calenderSize
                    [ Html.Style.positionAbsolute

                    --, Html.Style.leftPx (200 - (calenderSize / 2))
                    , Html.Style.bottomPx (0 - toFloat (i - args.day) * (calenderSize + toFloat 64))
                    , Html.Style.transition "bottom 1s"
                    ]
                    setting
                )
            )
        |> List.sortBy Tuple.first
        |> Html.Keyed.node "div"
            [ Html.Style.displayFlex

            --, Html.Style.gapPx 64
            --, Html.Style.heightPx 50
            --, Html.Style.widthPx 280
            --, Html.Style.leftPx 60
            --, Html.Style.top "50vh"
            , Html.Style.widthPx calenderSize
            , Html.Style.heightPx calenderSize

            --, Html.Style.height "100vh"
            , Html.Style.overflowHidden
            , Html.Style.positionRelative
            ]
    ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.alignItemsCenter
            , Html.Style.gapPx 32
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
            , Html.Style.justifyContentCenter
            , Html.Style.alignItemsCenter
            , Html.Style.overflowHidden
            ]
