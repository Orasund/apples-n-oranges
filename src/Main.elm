module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Style
import Layout
import View.Field
import View.Fruit


type alias FruitId =
    Int


type Fruit
    = Apple
    | Orange


type alias Model =
    { columns : Int
    , rows : Int
    , fruits : Dict FruitId Fruit
    , fields : Dict ( Int, Int ) FruitId
    , selected : Maybe ( Int, Int )
    }


type Msg
    = Click ( Int, Int )


init : () -> ( Model, Cmd Msg )
init () =
    ( { columns = 3
      , rows = 2
      , fruits = Dict.fromList [ ( 0, Apple ) ]
      , fields = Dict.fromList [ ( ( 2, 0 ), 0 ) ]
      , selected = Nothing
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    [ [ View.Field.toHtml
            [ View.Field.light ]
            { columns = model.columns, rows = model.rows }
      ]
    , model.fields
        |> Dict.toList
        |> List.filterMap
            (\( p, fruitId ) ->
                model.fruits
                    |> Dict.get fruitId
                    |> Maybe.map (Tuple.pair p)
            )
        |> List.map
            (\( ( x, y ), fruit ) ->
                View.Fruit.toHtml
                    ([ Html.Style.positionAbsolute
                     , Html.Style.topPx (toFloat y * View.Field.size)
                     , Html.Style.leftPx (toFloat x * View.Field.size)
                     ]
                        ++ (if model.selected == Just ( x, y ) then
                                [ View.Fruit.small ]

                            else
                                []
                           )
                    )
                    (case fruit of
                        Apple ->
                            View.Fruit.apple

                        Orange ->
                            View.Fruit.orange
                    )
            )
    , model.fields
        |> Dict.toList
        |> List.filterMap
            (\( p, fruitId ) ->
                model.fruits
                    |> Dict.get fruitId
                    |> Maybe.map (Tuple.pair p)
            )
        |> List.map
            (\( ( x, y ), fruit ) ->
                Html.div
                    (Layout.asButton
                        { onPress = Just (Click ( x, y ))
                        , label = "Select"
                        }
                        ++ [ Html.Style.aspectRatio "1"
                           , Html.Style.widthPx View.Field.size
                           , Html.Style.positionAbsolute
                           , Html.Style.topPx (toFloat y * View.Field.size)
                           , Html.Style.leftPx (toFloat x * View.Field.size)
                           ]
                    )
                    []
            )
    ]
        |> List.concat
        |> Html.div [ Html.Style.positionRelative ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pos ->
            ( case model.selected of
                Nothing ->
                    { model | selected = Just pos }

                Just p ->
                    if p == pos then
                        { model | selected = Nothing }

                    else
                        model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
