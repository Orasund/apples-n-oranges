module Ui exposing (main)

import Browser
import Html exposing (Html)
import Html.Style
import Layout
import Stylesheet
import View.Block
import View.Field


type alias Model =
    {}


type Msg
    = None


init : () -> ( Model, Cmd Msg )
init () =
    ( {}, Cmd.none )


view : Model -> Html Msg
view _ =
    [ [ [ View.Block.toHtml []
            View.Block.apple
        , View.Block.toHtml []
            View.Block.orange
        , View.Block.toHtml
            [ View.Block.rocking
            ]
            View.Block.apple
        , View.Block.toHtml
            [ View.Block.rocking
            ]
            View.Block.orange
        , View.Block.toHtml
            [ View.Block.small
            ]
            View.Block.apple
        , View.Block.toHtml
            [ View.Block.small
            ]
            View.Block.orange
        , View.Block.toHtml
            [ View.Block.small
            , View.Block.rocking
            ]
            View.Block.apple
        , View.Block.toHtml
            [ View.Block.small
            , View.Block.rocking
            ]
            View.Block.orange
        ]
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.flexDirectionRow
                ]
      , [ [ View.Field.toHtml
                [ View.Field.light ]
                { columns = 3, rows = 2 }
          , View.Block.toHtml
                [ Html.Style.positionAbsolute
                , Html.Style.topPx 0
                , Html.Style.leftPx View.Field.size
                ]
                View.Block.apple
          , View.Block.toHtml
                [ Html.Style.positionAbsolute
                , Html.Style.topPx 0
                , Html.Style.leftPx (2 * View.Field.size)
                ]
                View.Block.apple
          , View.Block.toHtml
                [ Html.Style.positionAbsolute
                , Html.Style.topPx View.Field.size
                , Html.Style.leftPx View.Field.size
                ]
                View.Block.orange
          , View.Block.toHtml
                [ Html.Style.positionAbsolute
                , Html.Style.topPx View.Field.size
                , Html.Style.leftPx (2 * View.Field.size)
                ]
                View.Block.orange
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
