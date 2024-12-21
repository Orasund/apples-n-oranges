module Screen.Game exposing (..)

import Data.ItemBag exposing (ItemBag)
import Html exposing (Html)
import Html.Style
import Level exposing (Level)
import View.Button
import View.Field
import View.Level


toHtml :
    { level : Level
    , items : ItemBag
    , showDot : Int
    , pointerZero : ( Float, Float )
    , onUndo : msg
    , onOpenMenu : msg
    , onPointerDown : { pos : ( Float, Float ), offset : ( Float, Float ) } -> msg
    , onPointerUp : ( Float, Float ) -> msg
    , onPointerEnd : ( Float, Float ) -> msg
    }
    -> Html msg
toHtml args =
    [ Html.div
        [ Html.Style.displayFlex
        , Html.Style.alignItemsEnd
        , Html.Style.justifyContentCenter
        , Html.Style.gapPx 4
        , Html.Style.width "100%"
        ]
        [ (if args.showDot > 0 then
            View.Button.withDot []
                { label = "Messages"
                , onPress = args.onOpenMenu
                , amount = args.showDot
                }

           else
            View.Button.toHtml []
                { label = "Messages"
                , onPress = args.onOpenMenu
                }
          )
            |> List.singleton
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                , Html.Style.justifyContentFlexEnd
                ]
        ]
    , View.Level.toHtml
        { game = args.level
        , items = args.items
        , onPointerDown = args.onPointerDown
        , onPointerUp = args.onPointerUp
        , onPointerEnd = args.onPointerEnd
        , zero = args.pointerZero
        }
    , View.Button.toHtml
        [ View.Button.primary
        , Html.Style.fontSizePx 24
        ]
        { label = "Undo"
        , onPress = args.onUndo
        }
        |> List.singleton
        |> Html.div
            [ Html.Style.flex "1"
            , Html.Style.displayFlex
            , Html.Style.justifyContentCenter
            ]
    ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.gapPx 16
            , Html.Style.widthPx (View.Field.size * 6)
            , Html.Style.positionRelative
            ]
