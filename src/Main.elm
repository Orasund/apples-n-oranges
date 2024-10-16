module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Game exposing (Fruit, FruitId, Game)
import Html exposing (Html)
import Html.Style
import Layout
import Level
import Maths
import Process
import Stylesheet
import Task
import View.Field
import View.Fruit


type Event
    = Join ( Int, Int ) ( Int, Int )


type alias Entity =
    { x : Float
    , y : Float
    , pos : ( Int, Int )
    , shrink : Bool
    }


type alias Model =
    { game : Game
    , entities : Dict FruitId Entity
    , events : List Event
    , level : Int
    }


type Msg
    = Click ( Int, Int )
    | LoadLevel Int


newEntity : ( Int, Int ) -> Entity
newEntity ( x, y ) =
    { x = toFloat x
    , y = toFloat y
    , pos = ( x, y )
    , shrink = False
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { game =
            Game.empty { columns = 3, rows = 2 }
      , entities = Dict.empty
      , events = []
      , level = 0
      }
        |> loadLevel 1
    , Cmd.none
    )


addFruit : ( Int, Int ) -> Fruit -> Model -> Model
addFruit ( x, y ) fruit model =
    let
        ( game, fruitId ) =
            model.game |> Game.addFruit ( x, y ) fruit
    in
    { model
        | game = game
        , entities =
            model.entities
                |> Dict.insert fruitId (newEntity ( x, y ))
    }


clearLevel : Model -> Model
clearLevel model =
    { model
        | game = Game.empty { columns = 0, rows = 0 }
        , entities = Dict.empty
        , events = []
    }


loadLevel : Int -> Model -> Model
loadLevel id model =
    let
        { columns, rows, fruits } =
            Level.toList id
    in
    fruits
        |> List.foldl
            (\( pos, fruit ) ->
                addFruit pos fruit
            )
            { model
                | level = id
                , game = Game.empty { columns = columns, rows = rows }
            }


viewFruit : { fruitId : FruitId, entity : Entity } -> Model -> String -> Html Msg
viewFruit args model =
    View.Fruit.toHtml
        ([ Html.Style.topPx (args.entity.y * View.Field.size)
         , Html.Style.leftPx (args.entity.x * View.Field.size)
         ]
            ++ (if args.entity.shrink then
                    [ View.Fruit.shrink ]

                else
                    model.game.selected
                        |> Maybe.map
                            (\selected ->
                                if selected == args.entity.pos then
                                    [ View.Fruit.small ]

                                else if Game.isValidPair args.entity.pos selected model.game then
                                    [ View.Fruit.rocking ]

                                else
                                    []
                            )
                        |> Maybe.withDefault []
               )
        )


view : Model -> Html Msg
view model =
    [ [ View.Field.toHtml
            [ View.Field.light ]
            { columns = model.game.columns, rows = model.game.rows }
      ]
    , model.entities
        |> Dict.toList
        |> List.filterMap
            (\( fruitId, entity ) ->
                model.game.fruits
                    |> Dict.get fruitId
                    |> Maybe.map
                        (\fruit ->
                            { fruitId = fruitId, entity = entity, fruit = fruit }
                        )
            )
        |> List.map
            (\{ fruitId, entity, fruit } ->
                viewFruit { fruitId = fruitId, entity = entity }
                    model
                    (case fruit of
                        Game.Apple ->
                            View.Fruit.apple

                        Game.Orange ->
                            View.Fruit.orange
                    )
            )
    , model.game.fields
        |> Dict.toList
        |> List.filterMap
            (\( p, fruitId ) ->
                model.game.fruits
                    |> Dict.get fruitId
                    |> Maybe.map (Tuple.pair p)
            )
        |> List.map
            (\( ( x, y ), fruit ) ->
                Html.div
                    (Layout.asButton
                        { onPress = Just (Click ( x, y ))
                        , label =
                            [ "Select "
                            , case fruit of
                                Game.Apple ->
                                    "Apple"

                                Game.Orange ->
                                    "Orange"
                            , " at "
                            , String.fromInt x
                            , ", "
                            , String.fromInt y
                            ]
                                |> String.concat
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
    , [ Html.button
            (Layout.asButton
                { onPress = Just (LoadLevel model.level), label = "Reset" }
            )
            [ Html.text "Reset" ]
      , Stylesheet.stylesheet
      ]
    ]
        |> List.concat
        |> Html.div [ Html.Style.positionRelative ]


join : ( Int, Int ) -> ( Int, Int ) -> Model -> Maybe Model
join p1 p2 model =
    let
        ( x, y ) =
            Maths.intersect p1 p2
    in
    Maybe.map2
        (\fruit1 fruit2 ->
            { model
                | game =
                    model.game
                        |> Game.removeField p1
                        |> Game.removeField p2
                        |> Game.setSelected Nothing
                , entities =
                    model.entities
                        |> Dict.update fruit1
                            (Maybe.map
                                (\entity ->
                                    { entity | x = x, y = y, shrink = True }
                                )
                            )
                        |> Dict.update fruit2
                            (Maybe.map
                                (\entity ->
                                    { entity | x = x, y = y, shrink = True }
                                )
                            )
                , events = Join p1 p2 :: model.events
            }
        )
        (Dict.get p1 model.game.fields)
        (Dict.get p2 model.game.fields)


checkWinCondition : Model -> ( Model, Cmd Msg )
checkWinCondition model =
    ( model
    , if Dict.isEmpty model.game.fields then
        Process.sleep 500
            |> Task.perform (\() -> LoadLevel (model.level + 1))

      else
        Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pos ->
            case model.game.selected of
                Nothing ->
                    ( { model | game = Game.setSelected (Just pos) model.game }, Cmd.none )

                Just p ->
                    if p == pos then
                        ( { model | game = Game.setSelected Nothing model.game }, Cmd.none )

                    else if Game.isValidPair pos p model.game then
                        join pos p model
                            |> Maybe.withDefault model
                            |> checkWinCondition

                    else
                        ( { model | game = Game.setSelected Nothing model.game }
                        , Cmd.none
                        )

        LoadLevel id ->
            ( model |> clearLevel |> loadLevel id, Cmd.none )


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
