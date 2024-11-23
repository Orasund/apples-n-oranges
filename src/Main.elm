module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Entity exposing (Entity)
import Event exposing (Event(..))
import Html exposing (Html)
import Html.Attributes
import Html.Style
import Level exposing (Block(..), BlockId, Fruit(..), Level, Puzzle, Solid(..))
import Maths
import Process
import Puzzle.Generator exposing (Setting)
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
    { game : Level
    , entities : Dict BlockId Entity
    , coins : Dict CoinId Coin
    , day : Int
    , money : Int
    , nextCoinId : CoinId
    , setting : Event
    , nextLevels : List Event
    , seed : Seed
    , history :
        List
            { game : Level
            , entities : Dict BlockId Entity
            , coins : Dict CoinId Coin
            , nextCoinId : CoinId
            }
    , possibleSettings : List Setting
    , endOfDay : Bool
    , shop :
        Maybe
            { buyableSettings : List Setting
            , selected : Maybe Int
            }
    }


type Msg
    = Click ( Int, Int )
    | CollectCoin CoinId
    | Undo
    | EndDay
    | SetSeed Seed
    | NextDay
    | LoadNextLevel
    | StartDay
    | OpenShop
      --| CloseShop
    | SelectSettingToBuy (Maybe Int)
    | BuySettingAndReplaceWith Int
    | Wait Float Msg
    | Sequence (List Msg)



--| BuySetting Setting


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


endDay : Model -> Model
endDay model =
    { model | endOfDay = True }


gotoLevel : Model -> Model
gotoLevel model =
    { model
        | endOfDay = False
    }


openShop : Model -> Random Model
openShop model =
    Puzzle.Generator.pickSettings
        { amount = 2
        , money = model.money
        }
        |> Random.map
            (\list ->
                { model
                    | shop =
                        Just
                            { buyableSettings = list
                            , selected = Nothing
                            }
                }
            )


closeShop : Model -> Model
closeShop model =
    { model | shop = Nothing }


nextDay : Model -> Model
nextDay model =
    { model | day = model.day + 1 }


loadPuzzle : Puzzle -> Model -> Model
loadPuzzle puzzle model =
    List.foldl
        (\( pos, fruit ) ->
            addBlock pos fruit
        )
        model
        puzzle.blocks


clearLevel : Model -> Model
clearLevel model =
    { model
        | game = Level.clear model.game
        , entities = Dict.empty
        , coins = Dict.empty
        , history = []
    }


loadNextLevel : Model -> Random Model
loadNextLevel model =
    case model.nextLevels of
        head :: tail ->
            case head of
                WeatherEvent weather ->
                    weather
                        |> Puzzle.Generator.generate model.game
                        |> Random.map
                            (\puzzle ->
                                { model
                                    | setting = head
                                    , nextLevels = tail
                                }
                                    |> clearLevel
                                    |> loadPuzzle puzzle
                            )

                ShopEvent ->
                    { model
                        | setting = head
                        , nextLevels = tail
                    }
                        |> clearLevel
                        |> openShop

        [] ->
            Random.constant model



--model
--    |> gotoShop


applyGenerator : Seed -> Random Model -> Model
applyGenerator seed generator =
    let
        ( model, newSeed ) =
            Random.step generator seed
    in
    { model | seed = newSeed }


shortWaitThenPerform : Msg -> Msg
shortWaitThenPerform =
    Wait 100


longWaitThenPerform : Msg -> Msg
longWaitThenPerform =
    Wait 1000


sequence : List Msg -> Cmd Msg
sequence list =
    Task.succeed ()
        |> Task.perform (\() -> Sequence list)


selectSettingToBuy : Maybe Int -> Model -> Model
selectSettingToBuy selected model =
    { model
        | shop =
            model.shop
                |> Maybe.map
                    (\shop ->
                        { shop | selected = selected }
                    )
    }


buyAndReplaceSetting : Int -> Model -> Model
buyAndReplaceSetting replaceWith model =
    model.shop
        |> Maybe.andThen
            (\shop ->
                shop.selected
                    |> Maybe.andThen
                        (\selected ->
                            shop.buyableSettings
                                |> List.drop selected
                                |> List.head
                        )
            )
        |> Maybe.map
            (\setting ->
                { model
                    | possibleSettings =
                        model.possibleSettings
                            |> List.indexedMap
                                (\j s ->
                                    if replaceWith == j then
                                        setting

                                    else
                                        s
                                )
                            |> List.sortBy .difficulty
                }
            )
        |> Maybe.withDefault model


generateNextWeek : Model -> Random Model
generateNextWeek model =
    model.possibleSettings
        |> Puzzle.Generator.sort
        |> Random.map
            (\nextLevels ->
                { model
                    | nextLevels =
                        List.map WeatherEvent nextLevels
                            ++ [ ShopEvent ]
                }
            )



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

        model : Model
        model =
            { game = Level.empty { columns = 6, rows = 6 }
            , entities = Dict.empty
            , coins = Dict.empty
            , money = 0
            , day = 1
            , nextCoinId = 0
            , setting = WeatherEvent Puzzle.Generator.startingLevel
            , nextLevels =
                List.map WeatherEvent Puzzle.Generator.tutorials
                    ++ [ ShopEvent ]
            , history = []
            , possibleSettings = Puzzle.Generator.tutorials
            , seed = seed
            , endOfDay = False
            , shop = Nothing
            }
    in
    ( model
        |> loadNextLevel
        |> applyGenerator seed
    , Random.generate SetSeed Random.independentSeed
    )


addBlock : ( Int, Int ) -> Block -> Model -> Model
addBlock ( x, y ) block model =
    let
        ( game, fruitId ) =
            model.game |> Level.addBlock ( x, y ) block
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
                    2

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
                        |> Level.removeField p1
                        |> Level.removeField p2
                        |> Level.setSelected Nothing
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
        (model.game |> Level.getBlockAndIdAt p1)
        (model.game |> Level.getBlockAndIdAt p2)


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
            sequence
                [ shortWaitThenPerform EndDay
                , longWaitThenPerform LoadNextLevel

                --  , longWaitThenPerform StartDay
                ]

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
                    case Level.getBlockAndIdAt pos model.game of
                        Just ( _, SolidBlock Sprout ) ->
                            ( model, Cmd.none )

                        _ ->
                            ( { model | game = Level.setSelected (Just pos) model.game }, Cmd.none )

                Just p ->
                    if p == pos then
                        ( { model | game = Level.setSelected Nothing model.game }, Cmd.none )

                    else if Level.isValidPair pos p model.game then
                        join pos p model
                            |> Maybe.withDefault model
                            |> checkWinCondition

                    else
                        ( { model | game = Level.setSelected Nothing model.game }
                        , Cmd.none
                        )

        Undo ->
            case model.history of
                history :: tail ->
                    ( { model
                        | game = history.game |> Level.setSelected Nothing
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

        EndDay ->
            ( model
                |> collectCoins
                |> endDay
              --    |> generateAnotherSetting
              --|> applyGenerator model.seed
            , Cmd.none
            )

        SetSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        LoadNextLevel ->
            model
                |> closeShop
                |> loadNextLevel
                |> (\m ->
                        ( m
                            |> Random.map nextDay
                            |> applyGenerator model.seed
                        , sequence [ longWaitThenPerform StartDay ]
                        )
                   )

        NextDay ->
            ( nextDay model, Cmd.none )

        StartDay ->
            ( gotoLevel model, Cmd.none )

        OpenShop ->
            ( model
                |> openShop
                |> applyGenerator model.seed
            , Cmd.none
            )

        {--CloseShop ->
            ( model
                |> generateAnotherSetting
                |> Random.map gotoLevel
                |> applyGenerator model.seed
            , sequence
                [ LoadNextLevel
                ]
            )--}
        SelectSettingToBuy i ->
            ( model |> selectSettingToBuy i, Cmd.none )

        BuySettingAndReplaceWith i ->
            ( model
                |> buyAndReplaceSetting i
                |> selectSettingToBuy Nothing
                --|> generateAnotherSetting
                |> generateNextWeek
                --   |> Random.map closeShop
                -- |> Random.map endDay
                |> applyGenerator model.seed
            , sequence
                [ shortWaitThenPerform EndDay
                , longWaitThenPerform LoadNextLevel
                ]
            )

        Wait ms m ->
            ( model
            , Process.sleep ms
                |> Task.perform (\() -> m)
            )

        Sequence list ->
            case list of
                [] ->
                    ( model, Cmd.none )

                head :: tail ->
                    update head model
                        |> Tuple.mapSecond
                            (\cmd ->
                                Cmd.batch
                                    [ cmd
                                    , Task.succeed ()
                                        |> Task.perform (\() -> Sequence tail)
                                    ]
                            )


view : Model -> Html Msg
view model =
    [ case model.shop of
        Nothing ->
            [ View.Header.viewHeader
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

        Just shop ->
            View.Shop.toHtml
                { settings = model.possibleSettings
                , buyableSettings = shop.buyableSettings
                , selected = shop.selected
                , onSelectSettingToBuy = SelectSettingToBuy
                , money = model.money

                --, onCloseShop = CloseShop
                , onBuy = BuySettingAndReplaceWith
                }
    , View.EndOfDay.toHtml
        { money = model.money
        , currentEvent = model.setting
        , nextEvents = model.nextLevels
        , endOfDay = model.endOfDay
        , day = model.day
        }
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
