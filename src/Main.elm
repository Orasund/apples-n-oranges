module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Game exposing (Block(..), BlockId, Fruit(..), Game, Solid(..))
import Generator
import Html exposing (Html)
import Html.Attributes
import Html.Keyed
import Html.Style
import Layout
import Level exposing (Level)
import Maths
import Process
import Random exposing (Seed)
import Stylesheet
import Task
import View.Block
import View.Coin
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
    , solids : Dict ( Int, Int ) Solid
    , money : Int
    , nextCoinId : CoinId
    , level : Int
    , levelDef : Level
    , seed : Seed
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
    | CollectCoin CoinId
    | GenerateLevel
    | Undo
    | Won
    | SetSeed Seed


priceToRemoveStone : ( Int, Int ) -> Int
priceToRemoveStone ( x, y ) =
    (7
        - round (abs (toFloat x - 2.5))
        - round (abs (toFloat y - 2.5))
    )
        ^ 2


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
            Game.empty { columns = 6, rows = 6 }
      , entities = Dict.empty
      , solids = Dict.empty
      , coins = Dict.empty
      , money = 0
      , nextCoinId = 0
      , level = 0
      , levelDef = Level.fromStrings []
      , history = []
      , seed = Random.initialSeed 42
      }
      --|> addBlock ( 2, 2 ) (SolidBlock Sprout)
      --|> addBlock ( 2, 3 ) (SolidBlock Sprout)
    , [ Task.perform
            (\() ->
                Won
            )
            (Task.succeed ())
      , Random.generate SetSeed Random.independentSeed
      ]
        |> Cmd.batch
    )


addBlock : ( Int, Int ) -> Block -> Model -> Model
addBlock ( x, y ) block model =
    let
        ( game, fruitId ) =
            model.game |> Game.addBlock ( x, y ) block
    in
    { model
        | game = game
        , entities =
            case block of
                SolidBlock _ ->
                    model.entities

                _ ->
                    model.entities
                        |> Dict.insert fruitId (newEntity ( x, y ))
    }


clearLevel : Model -> Model
clearLevel model =
    { model
        | game = Game.empty { columns = 0, rows = 0 }
        , entities = Dict.empty
        , coins = Dict.empty
        , history = []
    }


loadLevel : Int -> Level -> Model -> Model
loadLevel id level model =
    let
        { columns, rows, blocks } =
            level
    in
    blocks
        |> List.foldl
            (\( pos, fruit ) ->
                addBlock pos fruit
            )
            { model
                | level = id
                , levelDef = level
                , game = Game.empty { columns = columns, rows = rows }
                , solids =
                    blocks
                        |> List.filterMap
                            (\( pos, block ) ->
                                case block of
                                    SolidBlock solid ->
                                        Just ( pos, solid )

                                    FruitBlock _ ->
                                        Nothing
                            )
                        |> Dict.fromList
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


viewSolid : { x : Int, y : Int, solid : Solid } -> Model -> Html Msg
viewSolid args model =
    View.Block.withContent
        ([ Html.Style.topPx (toFloat args.y * View.Field.size)
         , Html.Style.leftPx (toFloat args.x * View.Field.size)
         ]
            ++ (model.game.selected
                    |> Maybe.map
                        (\selected ->
                            if selected == ( args.x, args.y ) then
                                [ View.Block.small ]

                            else if Game.isValidPair ( args.x, args.y ) selected model.game then
                                [ View.Block.rocking ]

                            else
                                []
                        )
                    |> Maybe.withDefault []
               )
        )
        [ Html.div
            [ Html.Style.positionAbsolute
            , Html.Style.bottomPx 8
            , Html.Style.width "100%"
            , Html.Style.displayFlex
            , Html.Style.justifyContentCenter
            , Html.Style.fontSizePx 10
            ]
            (case args.solid of
                Stone ->
                    if model.money >= priceToRemoveStone ( args.x, args.y ) then
                        [ Html.div
                            [ Html.Style.background "white"
                            , Html.Style.border "2px solid black"
                            , Html.Style.borderRadiusPx 16
                            , Html.Style.displayFlex
                            , Html.Style.alignItemsCenter
                            , Html.Style.paddingPx 2
                            ]
                            [ View.Coin.toHtml
                                [ Html.Style.fontSizePx 10
                                , Html.Style.heightPx 16
                                ]
                                (priceToRemoveStone ( args.x, args.y ))
                            , Html.div
                                [ Html.Style.padding "2px 4px"
                                ]
                                [ Html.text "Remove" ]
                            ]
                        ]

                    else
                        []

                Sprout ->
                    []
            )
        , Html.div []
            (case args.solid of
                Stone ->
                    [ Html.text View.Block.stone ]

                Sprout ->
                    [ Html.text View.Block.sprout ]
            )
        ]


viewMoney : Coin -> Html msg
viewMoney money =
    View.Block.withContent
        ([ Html.Style.topPx (money.y * View.Field.size)
         , Html.Style.leftPx (money.x * View.Field.size)
         , Html.Style.heightPx View.Field.size
         ]
            ++ (if money.shrink then
                    [ View.Block.shrink ]

                else
                    []
               )
        )
        [ View.Coin.toHtml
            [ Html.Style.fontSizePx 24
            , Html.Style.heightPx 40
            , Html.Style.borderWidthPx 4
            , Html.Style.displayFlex
            ]
            1
        ]


view : Model -> Html Msg
view model =
    [ Html.div
        [ Html.Style.displayFlex
        , Html.Style.alignItemsCenter
        , Html.Style.justifyContentCenter
        , Html.Style.gapPx 4
        ]
        [ Html.button
            (Layout.asButton
                { onPress = Just Undo, label = "Undo" }
                ++ [ Html.Attributes.class "button"
                   , Html.Style.paddingPx 2
                   , Html.Style.displayFlex
                   , Html.Style.alignItemsCenter
                   ]
            )
            [ View.Coin.toHtml
                [ Html.Style.fontSizePx 14
                , Html.Style.heightPx 26
                , Html.Style.borderWidthPx 3
                ]
                1
            , Html.div [ Html.Style.padding "4px 8px" ]
                [ Html.text "Undo" ]
            ]
        , View.Coin.toHtml
            [ Html.Style.fontSizePx 48
            , Html.Style.heightPx 100
            , Html.Style.borderWidthPx 8
            ]
            (model.money |> min 999 |> max -99)
        , Html.button
            (Layout.asButton
                { onPress = Just Undo, label = "Undo" }
                ++ [ Html.Attributes.class "button"
                   , Html.Style.padding "8px 16px"
                   , Html.Style.displayFlex
                   , Html.Style.gapPx 8
                   , Html.Style.alignItemsCenter
                   ]
            )
            [ View.Coin.toHtml
                [ Html.Style.fontSizePx 10
                , Html.Style.heightPx 14
                ]
                1
            , Html.text "Undo"
            ]
        ]
    , [ [ View.Field.toHtml
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

                                Game.FruitBlock Game.Lemon ->
                                    [ Html.text View.Block.lemon ]

                                Game.FruitBlock Game.Pear ->
                                    [ Html.text View.Block.pear ]

                                Game.SolidBlock _ ->
                                    []
                            )
                        )
                    )
          , [ model.coins
                |> Dict.toList

            {--,
              model.money
                |> Set.toList
                |> List.indexedMap
                    (\i coinId ->
                        ( coinId
                        , { x = toFloat i * 0.05
                          , y = -1
                          , shrink = False
                          }
                        )
                    )--}
            ]
                |> List.concat
                |> List.sortBy Tuple.first
                |> List.map
                    (\( id, coin ) ->
                        ( "coin_" ++ String.fromInt id
                        , viewMoney coin
                        )
                    )
          , model.solids
                |> Dict.toList
                |> List.map
                    (\( ( x, y ), solid ) ->
                        ( "solid_" ++ String.fromInt x ++ "_" ++ String.fromInt y
                        , viewSolid { x = x, y = y, solid = solid } model
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

                                    Game.FruitBlock Game.Lemon ->
                                        "Lemon"

                                    Game.FruitBlock Game.Pear ->
                                        "Pear"

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
    , Stylesheet.stylesheet
    , Html.node "meta"
        [ Html.Attributes.name "viewport"
        , Html.Attributes.attribute "content" "width=480, initial-scale=1,user-scalable=no"
        ]
        []
    ]
        |> Html.div [ Html.Style.widthPx 480 ]


join : ( Int, Int ) -> ( Int, Int ) -> Model -> Maybe Model
join p1 p2 model =
    let
        ( x, y ) =
            fromPolar ( 0.1, Maths.length p1 p2 )
                |> Maths.plus (Maths.intersect p1 p2)
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


generateLevel : Model -> Model
generateLevel model =
    Generator.generateLevel
        { columns = 6
        , rows = 6
        , oldBlocks = model.game |> Game.getBlocks |> Dict.fromList
        , newSprouts = 1 --model.level // 8 + 1
        , newFruitPairs = model.level // 8 + 1
        , newStone = model.level |> modBy 2
        }
        |> (\gen -> Random.step gen model.seed)
        |> (\( level, seed ) ->
                { model
                    | seed = seed
                }
                    |> clearAndloadLevel (model.level + 1) level
           )


clearAndloadLevel : Int -> Level -> Model -> Model
clearAndloadLevel id def model =
    model |> clearLevel |> loadLevel id def


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pos ->
            case model.game.selected of
                Nothing ->
                    case Game.getBlockAndIdAt pos model.game of
                        Just ( _, SolidBlock Stone ) ->
                            if model.money >= priceToRemoveStone pos then
                                ( { model
                                    | game = Game.removeField pos model.game
                                    , solids = model.solids |> Dict.remove pos
                                    , money = model.money - priceToRemoveStone pos
                                  }
                                , Cmd.none
                                )

                            else
                                ( model, Cmd.none )

                        Just ( _, SolidBlock Sprout ) ->
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

        GenerateLevel ->
            ( generateLevel model
            , Cmd.none
            )

        Undo ->
            case model.history of
                history :: tail ->
                    ( { model
                        | game = history.game |> Game.setSelected Nothing
                        , entities = history.entities
                        , coins = history.coins
                        , history = tail
                        , money = model.money - 1
                        , nextCoinId = history.nextCoinId
                      }
                    , Cmd.none
                    )

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
                | money = Dict.size model.coins + model.money
              }
            , Task.succeed () |> Task.perform (\() -> GenerateLevel)
            )

        SetSeed seed ->
            ( { model | seed = seed }, Cmd.none )


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
