module Ui exposing (main)

import Browser
import Html exposing (Html)
import Html.Style
import Layout
import Stylesheet
import View.Field
import View.Fruit


type alias Model =
    {}


type Msg
    = None


init : () -> ( Model, Cmd Msg )
init arg1 =
    ( {}, Cmd.none )


view : Model -> Html Msg
view arg1 =
    [ [ [ View.Fruit.toHtml []
            View.Fruit.apple
        , View.Fruit.toHtml []
            View.Fruit.orange
        , View.Fruit.toHtml
            [ View.Fruit.rocking
            ]
            View.Fruit.apple
        , View.Fruit.toHtml
            [ View.Fruit.rocking
            ]
            View.Fruit.orange
        , View.Fruit.toHtml
            [ View.Fruit.small
            ]
            View.Fruit.apple
        , View.Fruit.toHtml
            [ View.Fruit.small
            ]
            View.Fruit.orange
        , View.Fruit.toHtml
            [ View.Fruit.small
            , View.Fruit.rocking
            ]
            View.Fruit.apple
        , View.Fruit.toHtml
            [ View.Fruit.small
            , View.Fruit.rocking
            ]
            View.Fruit.orange
        ]
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.flexDirectionRow
                ]
      , [ [ View.Field.toHtml
                [ View.Field.light ]
                { columns = 3, rows = 2 }
          , View.Fruit.toHtml
                [ Html.Style.positionAbsolute
                , Html.Style.topPx 0
                , Html.Style.leftPx View.Field.size
                ]
                View.Fruit.apple
          , View.Fruit.toHtml
                [ Html.Style.positionAbsolute
                , Html.Style.topPx 0
                , Html.Style.leftPx (2 * View.Field.size)
                ]
                View.Fruit.apple
          , View.Fruit.toHtml
                [ Html.Style.positionAbsolute
                , Html.Style.topPx View.Field.size
                , Html.Style.leftPx View.Field.size
                ]
                View.Fruit.orange
          , View.Fruit.toHtml
                [ Html.Style.positionAbsolute
                , Html.Style.topPx View.Field.size
                , Html.Style.leftPx (2 * View.Field.size)
                ]
                View.Fruit.orange
          , Html.div
                (Layout.asButton
                    { onPress = Nothing
                    , label = "Test"
                    }
                    ++ [ Html.Style.aspectRatio "1"
                       , Html.Style.widthPx View.Field.size
                       , Html.Style.positionAbsolute
                       , Html.Style.topPx View.Field.size
                       , Html.Style.leftPx (2 * View.Field.size)
                       ]
                )
                []
          ]
            |> Html.div [ Html.Style.positionRelative ]
        ]
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.flexDirectionRow
                ]
      ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            ]
    , Stylesheet.stylesheet
    ]
        |> Html.div []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions arg1 =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
