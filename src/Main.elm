module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Game exposing (Block(..), BlockId, Game, Solid(..))
import Generator
import Html exposing (Html)
import Html.Attributes
import Html.Keyed
import Html.Style
import Layout
import Level exposing (Level)
import Maths
import Process
import Random
import Set exposing (Set)
import Stylesheet
import Task
import View.Block
import View.Field


type alias Entity =
    { x : Float
    , y : Float
    , pos : ( Int, Int )
    , shrink : Bool
    }


type alias Coin =
    { x : Float, y : Float, shrink : Bool }


type alias CoinId =
    Int


type alias Model =
    { game : Game
    , entities : Dict BlockId Entity
    , coins : Dict CoinId Coin
    , solids : List Solid
    , money : Set CoinId
    , nextCoinId : CoinId
    , level : Int
    , levelDef : Level
    , history :
        List
            { game : Game
            , entities : Dict BlockId Entity
            , coins : Dict CoinId Coin
            , nextCoinId : CoinId
            }
    }


type Msg
    = Click ( Int, Int )
    | LoadLevel ( Int, Level )
    | CollectCoin CoinId
    | GenerateLevel
    | Undo
    | Won


shouldGenerateLevel : Bool
shouldGenerateLevel =
    True


newEntity : ( Int, Int ) -> Entity
newEntity ( x, y ) =
    { x = toFloat x
    , y = toFloat y
    , pos = ( x, y )
    , shrink = False
    }


init : () -> ( Model, Cmd Msg )
init () =
    { game =
        Game.empty { columns = 2, rows = 2 }
    , entities = Dict.empty
    , solids = []
    , coins = Dict.empty
    , money = Set.empty
    , nextCoinId = 0
    , level = 0
    , levelDef = Level.fromStrings []
    , history = []
    }
        |> checkWinCondition


addBlock : ( Int, Int ) -> Block -> Model -> Model
addBlock ( x, y ) block model =
    let
        ( game, fruitId ) =
            model.game |> Game.addBlock ( x, y ) block
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
        , coins = Dict.empty
    }


loadLevel : Int -> Level -> Model -> Model
loadLevel id level model =
    let
        { columns, rows, fruits } =
            level
    in
    fruits
        |> List.foldl
            (\( pos, fruit ) ->
                addBlock pos fruit
            )
            { model
                | level = id
                , levelDef = level
                , game = Game.empty { columns = columns, rows = rows }
            }


viewFruit : { blockId : BlockId, entity : Entity } -> Model -> List (Html Msg) -> Html Msg
viewFruit args model =
    View.Block.withContent
        ([ Html.Style.topPx (args.entity.y * View.Field.size)
         , Html.Style.leftPx (args.entity.x * View.Field.size)
         ]
            ++ (if args.entity.shrink then
                    [ View.Block.shrink ]

                else
                    model.game.selected
                        |> Maybe.map
                            (\selected ->
                                if selected == args.entity.pos then
                                    [ View.Block.small ]

                                else if Game.isValidPair args.entity.pos selected model.game then
                                    [ View.Block.rocking ]

                                else
                                    []
                            )
                        |> Maybe.withDefault []
               )
        )


viewMoney : Coin -> Html msg
viewMoney money =
    View.Block.withContent
        ([ Html.Style.topPx (money.y * View.Field.size)
         , Html.Style.leftPx (money.x * View.Field.size)
         ]
            ++ (if money.shrink then
                    [ View.Block.shrink ]

                else
                    []
               )
        )
        [ Html.text "🪙" ]


view : Model -> Html Msg
view model =
    [ [ [ View.Field.toHtml
            [ View.Field.light ]
            { columns = model.game.columns, rows = model.game.rows }
        , [ model.entities
                |> Dict.toList
                |> List.filterMap
                    (\( blockId, entity ) ->
                        model.game.blocks
                            |> Dict.get blockId
                            |> Maybe.map
                                (\block ->
                                    { blockId = blockId, entity = entity, block = block }
                                )
                    )
                |> List.map
                    (\{ blockId, entity, block } ->
                        ( "block_" ++ String.fromInt blockId
                        , viewFruit { blockId = blockId, entity = entity }
                            model
                            (case block of
                                Game.FruitBlock Game.Apple ->
                                    [ Html.text View.Block.apple ]

                                Game.FruitBlock Game.Orange ->
                                    [ Html.text View.Block.orange ]

                                Game.SolidBlock Game.Pig ->
                                    [ Html.text View.Block.pig ]

                                Game.SolidBlock Game.Chicken ->
                                    [ Html.text View.Block.chicken ]

                                Game.SolidBlock Game.Cow ->
                                    [ Html.text View.Block.cow ]

                                Game.SolidBlock Game.Sheep ->
                                    [ Html.text View.Block.sheep ]

                                Game.SolidBlock Game.Stone ->
                                    [ Html.div
                                        [ Html.Style.positionAbsolute
                                        , Html.Style.bottomPx 8
                                        , Html.Style.width "100%"
                                        , Html.Style.displayFlex
                                        , Html.Style.justifyContentCenter
                                        , Html.Style.fontSizePx 10
                                        ]
                                        [ Html.div
                                            [ Html.Style.background "white"
                                            , Html.Style.border "2px solid black"
                                            , Html.Style.borderRadiusPx 16
                                            , Html.Style.padding "4px 8px"
                                            ]
                                            [ Html.text "🪙 Remove" ]
                                        ]
                                    , Html.div [] [ Html.text View.Block.stone ]
                                    ]
                            )
                        )
                    )
          , [ model.coins
                |> Dict.toList
            , model.money
                |> Set.toList
                |> List.indexedMap
                    (\i coinId ->
                        ( coinId
                        , { x = toFloat i * 0.05
                          , y = -1
                          , shrink = False
                          }
                        )
                    )
            ]
                |> List.concat
                |> List.sortBy Tuple.first
                |> List.map
                    (\( id, coin ) ->
                        ( "coin_" ++ String.fromInt id
                        , viewMoney coin
                        )
                    )
          ]
            |> List.concat
            |> Html.Keyed.node "div"
                [ Html.Style.positionAbsolute
                , Html.Style.topPx 0
                , Html.Style.leftPx 0
                ]
        ]
      , model.game.fields
            |> Dict.toList
            |> List.filterMap
                (\( p, fruitId ) ->
                    model.game.blocks
                        |> Dict.get fruitId
                        |> Maybe.map (Tuple.pair p)
                )
            |> List.map
                (\( ( x, y ), block ) ->
                    Html.div
                        (Layout.asButton
                            { onPress = Just (Click ( x, y ))
                            , label =
                                [ "Select "
                                , case block of
                                    Game.FruitBlock Game.Apple ->
                                        "Apple"

                                    Game.FruitBlock Game.Orange ->
                                        "Orange"

                                    Game.SolidBlock _ ->
                                        "Solid Block"
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
      ]
        |> List.concat
        |> Html.div [ Html.Style.positionRelative ]
    , Html.button
        (Layout.asButton
            { onPress = Just (LoadLevel ( model.level, model.levelDef )), label = "Reset" }
            ++ [ Html.Attributes.class "button"
               , Html.Style.padding "8px 16px"
               ]
        )
        [ Html.text "Reset" ]
    , Html.button
        (Layout.asButton
            { onPress = Just Undo, label = "Undo" }
            ++ [ Html.Attributes.class "button"
               , Html.Style.padding "8px 16px"
               ]
        )
        [ Html.text "🪙 Undo" ]
    , Stylesheet.stylesheet
    ]
        |> Html.div [ Html.Style.paddingTopPx 80 ]


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
                , coins =
                    model.coins
                        |> Dict.insert model.nextCoinId
                            { x = x, y = y, shrink = True }
                , nextCoinId = model.nextCoinId + 1
                , history =
                    { game = model.game
                    , entities = model.entities
                    , coins = model.coins
                    , nextCoinId = model.nextCoinId
                    }
                        :: model.history
            }
        )
        (Dict.get p1 model.game.fields)
        (Dict.get p2 model.game.fields)


checkWinCondition : Model -> ( Model, Cmd Msg )
checkWinCondition model =
    let
        hasWon =
            Dict.toList model.game.fields
                |> List.all
                    (\( _, blockId ) ->
                        case model.game.blocks |> Dict.get blockId of
                            Just (FruitBlock _) ->
                                False

                            Just (SolidBlock _) ->
                                True

                            Nothing ->
                                True
                    )
    in
    ( model
    , [ Process.sleep 100
            |> Task.perform (\() -> CollectCoin (model.nextCoinId - 1))
      , if hasWon then
            Process.sleep 500
                |> Task.perform
                    (\() ->
                        Won
                    )

        else
            Cmd.none
      ]
        |> Cmd.batch
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pos ->
            case model.game.selected of
                Nothing ->
                    case Game.getBlockAndIdAt pos model.game of
                        Just ( blockId, SolidBlock _ ) ->
                            if Set.isEmpty model.money |> not then
                                ( { model
                                    | game = Game.removeField pos model.game
                                    , entities =
                                        model.entities
                                            |> Dict.update blockId
                                                (Maybe.map
                                                    (\entity ->
                                                        { entity | shrink = True }
                                                    )
                                                )
                                    , money =
                                        model.money
                                            |> Set.toList
                                            |> List.tail
                                            |> Maybe.withDefault []
                                            |> Set.fromList
                                  }
                                , Cmd.none
                                )

                            else
                                ( model, Cmd.none )

                        _ ->
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

        LoadLevel ( id, def ) ->
            ( model |> clearLevel |> loadLevel id def, Cmd.none )

        GenerateLevel ->
            ( model
            , Generator.generate
                { pairs = model.level + 1
                , solids = model.level + 1
                , columns = 6 --(model.level + 1) // 2 + 2
                , rows = 6 --(model.level + 1) // 2 + 2
                }
                |> Random.generate (\def -> LoadLevel ( model.level + 1, def ))
            )

        Undo ->
            case model.history of
                history :: tail ->
                    case model.money |> Set.toList of
                        _ :: money ->
                            ( { model
                                | game = history.game |> Game.setSelected Nothing
                                , entities = history.entities
                                , coins = history.coins
                                , history = tail
                                , money = Set.fromList money
                                , nextCoinId = history.nextCoinId
                              }
                            , Cmd.none
                            )

                        [] ->
                            ( model, Cmd.none )

                [] ->
                    ( model, Cmd.none )

        CollectCoin coinId ->
            ( { model
                | coins =
                    model.coins
                        |> Dict.update coinId
                            (Maybe.map
                                (\coin ->
                                    { coin
                                        | shrink = False
                                    }
                                )
                            )
              }
            , Cmd.none
            )

        Won ->
            ( { model
                | money =
                    model.coins
                        |> Dict.keys
                        |> Set.fromList
                        |> Set.union model.money
              }
            , Task.succeed ()
                |> Task.perform
                    (\() ->
                        if shouldGenerateLevel then
                            GenerateLevel

                        else
                            LoadLevel ( model.level + 1, Level.toList (model.level + 1) )
                    )
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
