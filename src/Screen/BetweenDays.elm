module Screen.BetweenDays exposing (..)

import Data.Block exposing (Block(..), Item)
import Data.Date as Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Keyed
import Html.Style
import Puzzle.Setting exposing (Event)
import View.Background
import View.CalenderDay
import View.DayOfTheWeek


type BetweenDaysAction
    = ShowCalenderDay
    | AdvanceCalenderDay
    | ShowNothing
    | ShowItemAdded Item
    | ShowItemRemoved Item
    | ShowMail
    | ShowYear
    | AdvanceYear
    | ShowEndscreen


showEndscreen : Html msg
showEndscreen =
    [ "You reached the end of"
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 30 ]
    , "Year 3"
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 100 ]
    , "Thanks for playing"
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 30 ]
    ]
        |> default []


showNothing : Html msg
showNothing =
    [] |> default []


showMail : Html msg
showMail =
    [ "You got Mail"
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 50 ]
    , "✉️"
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 200 ]
    ]
        |> default []


showItemAdded : { item : Item } -> Html msg
showItemAdded args =
    [ "Added"
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 75 ]
    , Data.Block.toString (ItemBlock args.item)
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 200 ]
    ]
        |> default []


showItemRemoved : { item : Item } -> Html msg
showItemRemoved args =
    [ "Removed"
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 75 ]
    , Data.Block.toString (ItemBlock args.item)
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 200 ]
    ]
        |> default []


showYear : { year : Int } -> Html msg
showYear args =
    [ "Year"
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 75 ]
    , String.fromInt (args.year + 1)
        |> Html.text
        |> List.singleton
        |> Html.div [ Html.Style.fontSizePx 200 ]
    ]
        |> default []


showCalenderDay :
    { nextEvents : Dict Date Event
    , date : Date
    }
    -> Html msg
showCalenderDay args =
    let
        calenderSize =
            300
    in
    [ Html.div [ Html.Style.fontSizePx 60 ] [ Html.text (View.DayOfTheWeek.toLongString (Date.day args.date)) ]
    , [ args.date, args.date |> Date.next ]
        |> List.map
            (\date ->
                args.nextEvents
                    |> Dict.get date
                    |> Maybe.map (\event -> ( Date.day date, event ))
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
                    , Html.Style.bottomPx (0 - toFloat (i - Date.day args.date) * (calenderSize + toFloat 64))
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
        |> default []


default : List (Attribute msg) -> List (Html msg) -> Html msg
default =
    withBackground View.Background.endOfDay


withBackground : (List (Attribute msg) -> List (Html a) -> b) -> List (Attribute msg) -> List (Html a) -> b
withBackground background attrs content =
    content
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.alignItemsCenter
            , Html.Style.gapPx 32
            ]
        |> List.singleton
        |> background
            ([ Html.Style.positionAbsolute
             , Html.Style.height "100vh"
             , Html.Style.width "100%"
             , Html.Style.displayFlex
             , Html.Style.justifyContentCenter
             , Html.Style.alignItemsCenter
             , Html.Style.overflowHidden
             ]
                ++ attrs
            )


toHtml : { show : Bool } -> List (Html msg) -> Html msg
toHtml args content =
    content
        |> Html.div
            [ Html.Style.positionAbsolute
            , Html.Style.height "100vh"
            , Html.Style.width "100%"
            , Html.Style.transition "bottom 1s"
            , Html.Style.leftPx 0
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
