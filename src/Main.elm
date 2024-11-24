module Main exposing (main)

import Browser
import Dict
import Event exposing (Event(..))
import Html exposing (Html)
import Html.Attributes
import Html.Style
import Level exposing (Block(..), CoinId, Fruit(..), Level, Puzzle, Solid(..))
import Maths
import Process
import Puzzle.Generator exposing (Setting)
import Random exposing (Generator, Seed)
import Stylesheet
import Task
import View.Background
import View.EndOfDay
import View.Game
import View.Header
import View.Shop


type alias Random a =
    Generator a


type alias Model =
    { level : Level
    , day : Int
    , money : Int
    , event : Event
    , nextEvents : List Event
    , seed : Seed
    , history : List Level
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
    | SelectSettingToBuy (Maybe Int)
    | BuySettingAndReplaceWith Int
    | Wait Float Msg
    | Sequence (List Msg)



--| BuySetting Setting


collectCoins : Model -> Model
collectCoins model =
    let
        ( level, amount ) =
            model.level
                |> Level.collectCoins
    in
    { model
        | money = amount + model.money
        , level = level
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
        | level = Level.clear model.level
        , history = []
    }


loadNextLevel : Model -> Random Model
loadNextLevel model =
    case model.nextEvents of
        head :: tail ->
            case head of
                WeatherEvent weather ->
                    weather
                        |> Puzzle.Generator.generate model.level
                        |> Random.map
                            (\puzzle ->
                                { model
                                    | event = head
                                    , nextEvents = tail
                                }
                                    |> clearLevel
                                    |> loadPuzzle puzzle
                            )

                ShopEvent ->
                    { model
                        | event = head
                        , nextEvents = tail
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
                    | nextEvents =
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


init : () -> ( Model, Cmd Msg )
init () =
    let
        seed =
            Random.initialSeed 42

        model : Model
        model =
            { level = Level.empty { columns = 6, rows = 6 }
            , money = 0
            , day = 1
            , event = WeatherEvent Puzzle.Generator.startingLevel
            , nextEvents =
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
        level =
            model.level |> Level.addBlock ( x, y ) block
    in
    { model
        | level = level
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
                | level =
                    model.level
                        |> Level.removeField p1
                        |> Level.moveEntity fruit1
                            { x = x, y = y, shrink = True }
                        |> Level.removeField p2
                        |> Level.moveEntity fruit2
                            { x = x, y = y, shrink = True }
                        |> Level.setSelected Nothing
                        |> Level.addCoin ( x, y ) (coinsEarnedFromMatching block1 block2)
                , history = model.level :: model.history
            }
        )
        (model.level |> Level.getBlockAndIdAt p1)
        (model.level |> Level.getBlockAndIdAt p2)


checkWinCondition : Model -> ( Model, Cmd Msg )
checkWinCondition model =
    let
        hasWon =
            Dict.toList model.level.fields
                |> List.all
                    (\( _, blockId ) ->
                        case model.level.blocks |> Dict.get blockId of
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
            |> Task.perform (\() -> CollectCoin (model.level.nextCoinId - 1))
      , if hasWon then
            sequence
                [ shortWaitThenPerform EndDay
                , longWaitThenPerform LoadNextLevel
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
            case model.level.selected of
                Nothing ->
                    case Level.getBlockAndIdAt pos model.level of
                        Just ( _, SolidBlock Sprout ) ->
                            ( model, Cmd.none )

                        _ ->
                            ( { model | level = Level.setSelected (Just pos) model.level }, Cmd.none )

                Just p ->
                    if p == pos then
                        ( { model | level = Level.setSelected Nothing model.level }, Cmd.none )

                    else if Level.isValidPair pos p model.level then
                        join pos p model
                            |> Maybe.withDefault model
                            |> checkWinCondition

                    else
                        ( { model | level = Level.setSelected Nothing model.level }
                        , Cmd.none
                        )

        Undo ->
            case model.history of
                history :: tail ->
                    ( { model
                        | level = history |> Level.setSelected Nothing
                        , history = tail
                        , money = model.money - 1
                      }
                    , Cmd.none
                    )

                [] ->
                    ( model, Cmd.none )

        CollectCoin coinId ->
            ( { model
                | level =
                    model.level
                        |> Level.showCoin coinId
              }
            , Cmd.none
            )

        EndDay ->
            ( model
                |> collectCoins
                |> endDay
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

        SelectSettingToBuy i ->
            ( model |> selectSettingToBuy i, Cmd.none )

        BuySettingAndReplaceWith i ->
            ( model
                |> buyAndReplaceSetting i
                |> selectSettingToBuy Nothing
                |> generateNextWeek
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
                { game = model.level
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
                , onBuy = BuySettingAndReplaceWith
                }
    , View.EndOfDay.toHtml
        { money = model.money
        , currentEvent = model.event
        , nextEvents = model.nextEvents
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
