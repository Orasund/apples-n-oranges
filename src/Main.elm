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
    { x : Float, y : Float, shrink : Bool, value : Int }


type alias CoinId =
    Int


type alias Model =
    { game : Game
    , entities : Dict BlockId Entity
    , coins : Dict CoinId Coin
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
    (8
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
    ( { game = Game.empty { columns = 6, rows = 6 }
      , entities = Dict.empty
      , coins = Dict.empty
      , money = 0
      , nextCoinId = 0
      , level = 0
      , levelDef = Level.fromStrings []
      , history = []
      , seed = Random.initialSeed 42
      }
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
                    []

                Sprout ->
                    []

                Dynamite ->
                    []
            )
        , Html.div []
            [ Html.text
                (case args.solid of
                    Stone ->
                        View.Block.stone

                    Sprout ->
                        View.Block.sprout

                    Dynamite ->
                        View.Block.dynamite
                )
            ]
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
            [ Html.Style.fontSizePx (View.Field.size / 4)
            , Html.Style.heightPx (View.Field.size / 2)
            , Html.Style.borderWidthPx 4
            , Html.Style.displayFlex
            ]
            money.value
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    Html.div
        [ Html.Style.displayFlex
        , Html.Style.alignItemsCenter
        , Html.Style.justifyContentCenter
        , Html.Style.gapPx 4
        , Html.Style.width "100%"
        ]
        [ [ View.Coin.toHtml
                [ Html.Style.fontSizePx 14
                , Html.Style.heightPx 26
                , Html.Style.borderWidthPx 3
                ]
                1
          , Html.div [ Html.Style.padding "4px 8px" ]
                [ Html.text "Undo" ]
          ]
            |> Html.button
                (Layout.asButton
                    { onPress = Just Undo, label = "Undo" }
                    ++ [ Html.Attributes.class "button"
                       , Html.Style.paddingPx 2
                       , Html.Style.displayFlex
                       , Html.Style.alignItemsCenter
                       ]
                )
            |> List.singleton
            |> Html.div [ Html.Style.flex "1" ]
        , View.Coin.toHtml
            [ Html.Style.fontSizePx 48
            , Html.Style.heightPx 100
            , Html.Style.borderWidthPx 8
            ]
            (model.money |> min 999 |> max -99)
            |> List.singleton
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                , Html.Style.justifyContentCenter
                ]
        , Html.div [ Html.Style.flex "1" ] []
        ]


view : Model -> Html Msg
view model =
    [ viewHeader model
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
                            [ Html.text
                                (case block of
                                    Game.FruitBlock Game.Apple ->
                                        View.Block.apple

                                    Game.FruitBlock Game.Orange ->
                                        View.Block.orange

                                    Game.FruitBlock Game.Lemon ->
                                        View.Block.lemon

                                    Game.FruitBlock Game.Grapes ->
                                        View.Block.grapes

                                    Game.SolidBlock Stone ->
                                        View.Block.stone

                                    Game.SolidBlock Sprout ->
                                        View.Block.sprout

                                    Game.SolidBlock Dynamite ->
                                        View.Block.dynamite
                                )
                            ]
                        )
                    )
          , model.coins
                |> Dict.toList
                |> List.singleton
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
                (\( ( x, y ), _ ) ->
                    Html.div
                        (Layout.asButton
                            { onPress = Just (Click ( x, y ))
                            , label =
                                [ "Select "
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
        , Html.Attributes.attribute "content" "width=400, initial-scale=1,user-scalable=no"
        ]
        []
    ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.alignItemsCenter
            , Html.Style.gapPx 16
            ]


coinsEarnedFromMatching : Block -> Block -> Int
coinsEarnedFromMatching block1 block2 =
    let
        coinsEarned block =
            case block of
                FruitBlock _ ->
                    1

                SolidBlock Stone ->
                    4

                SolidBlock _ ->
                    0
    in
    (coinsEarned block1 + coinsEarned block2) // 2


join : ( Int, Int ) -> ( Int, Int ) -> Model -> Maybe Model
join p1 p2 model =
    let
        ( x, y ) =
            fromPolar ( 0.1, Maths.distance p1 p2 )
                |> Maths.plus (Maths.intersect p1 p2)
    in
    Maybe.map2
        (\( fruit1, block1 ) ( fruit2, block2 ) ->
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
                            { x = x
                            , y = y
                            , shrink = True
                            , value = coinsEarnedFromMatching block1 block2
                            }
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
        (model.game |> Game.getBlockAndIdAt p1)
        (model.game |> Game.getBlockAndIdAt p2)


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
    let
        oldBlocks =
            model.game |> Game.getBlocks |> Dict.fromList

        beginnerFriendly =
            min (model.level // 2 + 1)
    in
    Random.uniform
        { newFruitPairs = 5 |> beginnerFriendly
        , newStone = 1 |> min (Dict.size oldBlocks - 8)
        , newDynamite = 1
        , newLemonPairs = 1 |> beginnerFriendly
        , newGrapePairs = 0
        }
        [ { newFruitPairs = 10 |> beginnerFriendly
          , newStone = 1 |> min (Dict.size oldBlocks - 8)
          , newDynamite = 1
          , newLemonPairs = 0
          , newGrapePairs = 0
          }
        , { newFruitPairs = 0
          , newStone = 1 |> min (Dict.size oldBlocks - 8)
          , newDynamite = 1
          , newLemonPairs = 10 |> beginnerFriendly
          , newGrapePairs = 0
          }
        , { newFruitPairs = 4 |> beginnerFriendly
          , newStone = 10 |> beginnerFriendly
          , newDynamite = 8 |> beginnerFriendly
          , newLemonPairs = 0 |> beginnerFriendly
          , newGrapePairs = 0
          }
        , { newFruitPairs = 1 |> beginnerFriendly
          , newStone = 0
          , newDynamite = 0
          , newLemonPairs = 4 |> beginnerFriendly
          , newGrapePairs = 5 |> beginnerFriendly
          }
        ]
        |> Random.andThen
            (\args ->
                Generator.generateLevel
                    { columns = 6
                    , rows = 6
                    , oldBlocks = oldBlocks
                    , newSprouts = 0
                    , newFruitPairs = args.newFruitPairs
                    , newStone = args.newStone
                    , newDynamite = args.newDynamite
                    , newLemonPairs = args.newLemonPairs
                    , newGrapePairs = args.newGrapePairs
                    }
            )
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
                | money =
                    model.coins
                        |> Dict.values
                        |> List.map .value
                        |> List.sum
                        |> (+) model.money
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
