module Screen.Game exposing (..)

import Data.ItemBag exposing (ItemBag)
import Html exposing (Html)
import Html.Style
import Level exposing (Level)
import Screen.Menu exposing (MenuTab)
import View.Button
import View.Field
import View.Level


toHtml :
    { level : Level
    , items : ItemBag
    , unansweredMessages : Int
    , pointerZero : ( Float, Float )
    , onUndo : msg
    , onReset : msg
    , onPointerDown : { pos : ( Float, Float ), offset : ( Float, Float ) } -> msg
    , onPointerUp : ( Float, Float ) -> msg
    , onPointerEnd : ( Float, Float ) -> msg
    , onSelectTab : MenuTab -> msg
    }
    -> Html msg
toHtml args =
    [ Screen.Menu.asTabs
        { selected = Nothing
        , onSelectTab = args.onSelectTab
        , unansweredMessages = args.unansweredMessages
        }
    , View.Level.toHtml
        { game = args.level
        , items = args.items
        , onPointerDown = args.onPointerDown
        , onPointerUp = args.onPointerUp
        , onPointerEnd = args.onPointerEnd
        , zero = args.pointerZero
        }
    , [ View.Button.toHtml []
            { label = "Reset"
            , onPress = args.onReset
            }
            |> List.singleton
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                , Html.Style.alignItemsCenter
                ]
      , View.Button.toHtml
            (View.Button.big
                ++ [ View.Button.primary
                   ]
            )
            { label = "Undo"
            , onPress = args.onUndo
            }
      , Html.div [ Html.Style.flex "1" ] []
      ]
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
            , Html.Style.widthPx (toFloat args.level.columns * View.Field.size)
            , Html.Style.positionRelative
            ]
