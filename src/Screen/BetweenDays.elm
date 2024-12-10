module Screen.BetweenDays exposing (..)

import Data.Block exposing (Block(..), Item)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Keyed
import Html.Style
import Puzzle.Setting exposing (Event)
import View.Background
import View.CalenderDay
import View.DayOfTheWeek


type BetweenDaysAction
    = ShowCalenderDay
    | ShowNothing
    | ShowFoundItem Item


showNothing : { show : Bool } -> Html msg
showNothing args =
    [] |> toHtml { show = args.show }


showFoundItem : { show : Bool, item : Item } -> Html msg
showFoundItem args =
    [ "Found"
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 75 ]
    , Data.Block.toString (ItemBlock args.item)
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 200 ]
    ]
        |> toHtml { show = args.show }


showCalenderDay :
    { nextEvents : Dict Int Event
    , day : Int
    , show : Bool
    }
    -> Html msg
showCalenderDay args =
    let
        calenderSize =
            300
    in
    [ Html.div [ Html.Style.fontSizePx 60 ] [ Html.text (View.DayOfTheWeek.toLongString args.day) ]
    , [ args.day, args.day + 1 ]
        |> List.map
            (\i ->
                args.nextEvents
                    |> Dict.get i
                    |> Maybe.map (\event -> ( i, event ))
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
            )
        |> List.concat
        |> List.map
            (\( i, event ) ->
                ( String.fromInt i
                , View.CalenderDay.calenderDay
                    { size = calenderSize
                    , day = i
                    }
                    [ Html.Style.positionAbsolute
                    , Html.Style.bottomPx (0 - toFloat (i - args.day) * (calenderSize + toFloat 64))
                    , Html.Style.transition "bottom 1s"
                    ]
                    event
                )
            )
        |> List.sortBy Tuple.first
        |> Html.Keyed.node "div"
            [ Html.Style.displayFlex
            , Html.Style.widthPx calenderSize
            , Html.Style.heightPx calenderSize
            , Html.Style.overflowHidden
            , Html.Style.positionRelative
            ]
    ]
        |> toHtml { show = args.show }


toHtml : { show : Bool } -> List (Html msg) -> Html msg
toHtml args content =
    content
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
                (if args.show then
                    "0vh"

                 else
                    "100vh"
                )
            , Html.Style.displayFlex
            , Html.Style.justifyContentCenter
            , Html.Style.alignItemsCenter
            , Html.Style.overflowHidden
            ]
