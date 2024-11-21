module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Entity exposing (Entity)
import Game exposing (Block(..), BlockId, Fruit(..), Game, Solid(..))
import Html exposing (Html)
import Html.Attributes
import Html.Style
import Level.Generator exposing (Setting)
import Maths
import Process
import Random exposing (Generator, Seed)
import Stylesheet
import Task
import View.Background
import View.Coin exposing (Coin, CoinId)
import View.EndOfDay
import View.Game
import View.Header
import View.Shop


type alias Random a =
    Generator a


type alias Model =
    { game : Game
    , entities : Dict BlockId Entity
    , coins : Dict CoinId Coin
    , day : Int
    , money : Int
    , nextCoinId : CoinId
    , level : Setting
    , nextLevels : List Setting
    , seed : Seed
    , history :
        List
            { game : Game
            , entities : Dict BlockId Entity
            , coins : Dict CoinId Coin
            , nextCoinId : CoinId
            }
    , possibleSettings : List Setting
    , endOfDay : Bool
    , openShop : Bool
    }


type Msg
    = Click ( Int, Int )
    | CollectCoin CoinId
    | Undo
    | Won
    | SetSeed Seed
    | NextDay
    | LoadNextLevel
    | StartDay
    | OpenShop


collectCoins : Model -> Model
collectCoins model =
    { model
        | money =
            model.coins
                |> Dict.values
                |> List.map .value
                |> List.sum
                |> (+) model.money
        , coins = Dict.map (\_ coin -> { coin | x = 2.5, y = -1 }) model.coins
    }


generateAnotherSetting : Model -> Generator Model
generateAnotherSetting model =
    Level.Generator.pickSetting
        { money = (model.money * 2) // 3
        }
        |> Random.map
            (\generatedSetting ->
                { model | nextLevels = model.nextLevels ++ [ generatedSetting ] }
            )


endDay : Model -> Model
endDay model =
    { model | endOfDay = True }


gotoLevel : Model -> Model
gotoLevel model =
    { model
        | endOfDay = False
        , openShop = False
    }


gotoShop : Model -> Model
gotoShop model =
    { model | openShop = True }


nextDay : Model -> Model
nextDay model =
    { model | day = model.day + 1 }


loadNextLevel : Model -> Random Model
loadNextLevel model =
    case model.nextLevels of
        head :: tail ->
            head
                |> Level.Generator.generate model.game
                |> Random.map
                    (\level ->
                        List.foldl
                            (\( pos, fruit ) ->
                                addBlock pos fruit
                            )
                            { model
                                | level = head
                                , game = Game.empty { columns = level.columns, rows = level.rows }
                                , day = model.day + 1
                                , entities = Dict.empty
                                , coins = Dict.empty
                                , nextLevels = tail
                                , history = []
                            }
                            level.blocks
                    )

        [] ->
            Random.constant model


applyGenerator : Seed -> Random Model -> Model
applyGenerator seed generator =
    let
        ( model, newSeed ) =
            Random.step generator seed
    in
    { model | seed = newSeed }


shortWaitThenPerform : msg -> Cmd msg
shortWaitThenPerform msg =
    Task.perform (\() -> msg) (Process.sleep 100)


longWaitThenPerform : msg -> Cmd msg
longWaitThenPerform msg =
    Task.perform (\() -> msg) (Process.sleep 1000)



{------------------------------------------------------------
 -
 - U   U  PPPP   DDDD    AAA   TTTTT  EEEEE
 - U   U  P   P  D   D  A   A    T    E
 - U   U  PPPP   D   D  AAAAA    T    EEE
 - UUUUU  P      DDDD   A   A    T    EEEEE
 -
 -----------------------------------------------------------}


newEntity : ( Int, Int ) -> Entity
newEntity ( x, y ) =
    { x = toFloat x
    , y = toFloat y
    , pos = ( x, y )
    , shrink = False
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        seed =
            Random.initialSeed 42
    in
    ( { game = Game.empty { columns = 6, rows = 6 }
      , entities = Dict.empty
      , coins = Dict.empty
      , money = 0
      , day = 0
      , nextCoinId = 0
      , level = Level.Generator.trainingGround1
      , nextLevels = Level.Generator.tutorials
      , history = []
      , possibleSettings = []
      , seed = seed
      , endOfDay = False
      , openShop = False
      }
        |> loadNextLevel
        |> applyGenerator seed
    , Random.generate SetSeed Random.independentSeed
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
            ( model
                |> collectCoins
                |> endDay
                |> generateAnotherSetting
                |> applyGenerator model.seed
            , longWaitThenPerform LoadNextLevel
            )

        SetSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        LoadNextLevel ->
            ( model
                |> loadNextLevel
                |> applyGenerator model.seed
            , longWaitThenPerform StartDay
            )

        NextDay ->
            ( nextDay model, Cmd.none )

        StartDay ->
            ( gotoLevel model, Cmd.none )

        OpenShop ->
            ( model |> gotoShop, Cmd.none )


view : Model -> Html Msg
view model =
    [ [ View.Header.viewHeader
            { money = model.money
            , onUndo = Undo
            , onOpenShop = OpenShop
            }
      , View.Game.viewGame
            { game = model.game
            , coins = model.coins
            , entities = model.entities
            , onClick = Click
            }
      ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.gapPx 16
            ]
    , View.EndOfDay.viewShop
        { money = model.money
        , currentLevel = model.level
        , nextLevels = model.nextLevels
        , endOfDay = model.endOfDay
        , day = model.day
        }
    , if model.openShop then
        View.Shop.toHtml

      else
        Html.div [] []
    , Stylesheet.stylesheet
    , Html.node "meta"
        [ Html.Attributes.name "viewport"
        , Html.Attributes.attribute "content" "width=400, initial-scale=1,user-scalable=no"
        ]
        []
    ]
        |> View.Background.game
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.alignItemsCenter
            , Html.Style.justifyContentCenter
            , Html.Style.positionRelative
            , Html.Style.gapPx 16
            , Html.Style.width "100%"
            , Html.Style.height "100%"
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
