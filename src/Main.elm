module Main exposing (main)

import Browser
import Data.Artefact exposing (Artifact)
import Data.Block exposing (Block(..), Optional(..))
import Dict exposing (Dict)
import Event exposing (Event(..))
import Html exposing (Html)
import Html.Attributes
import Html.Style
import Level exposing (CoinId, Level, Puzzle)
import Maths
import Process
import Puzzle.Builder exposing (Group(..))
import Puzzle.Setting
import Random exposing (Generator, Seed)
import Screen.BetweenDays exposing (BetweenDaysAction(..))
import Screen.Menu
import Stylesheet
import Task
import View.Background
import View.Button
import View.Field
import View.Game
import View.Header


type alias Random a =
    Generator a


type alias Model =
    { level : Level
    , difficutly : Float
    , day : Int
    , nextEvents : Dict Int Event
    , seed : Seed
    , history : List Level
    , betweenDays : BetweenDaysAction
    , showBetweenDays : Bool
    , summer : Bool
    , shop : Bool
    , showCalender : Bool
    , showPresent : Maybe Artifact
    , year : Int
    , money : Int
    , items : List Optional
    }


type Msg
    = Click ( Int, Int )
    | ShowCoins (List CoinId)
    | Undo
    | EndDay
    | SetSeed Seed
    | NextDay
    | LoadNextLevel
    | SetBetweenDays BetweenDaysAction
    | StartDay
    | OpenShop
    | Buy Optional
    | CloseShop
    | ApplyThenWait { now : Msg, wait : Float, andThen : Msg }
    | OpenCalender
    | CloseCalender
    | AcceptCoin
    | DoNothing


setBetweenDays : BetweenDaysAction -> Model -> Model
setBetweenDays action model =
    { model | betweenDays = action }


showBetweenDays : Model -> Model
showBetweenDays model =
    { model | showBetweenDays = True }


hideBetweenDays : Model -> Model
hideBetweenDays model =
    { model | showBetweenDays = False }


openShop : Model -> Model
openShop model =
    { model
        | shop = True
    }


closeShop : Model -> Model
closeShop model =
    { model | shop = False }


nextDay : Model -> Model
nextDay model =
    { model | day = model.day + 1 }


loadPuzzle : Puzzle -> Model -> Random Model
loadPuzzle puzzle model =
    List.foldl
        (\( pos, fruit ) ->
            Random.map (addBlock pos fruit)
        )
        (Random.constant model)
        puzzle.blocks


clearHistory : Model -> Model
clearHistory model =
    { model
        | level = Level.clear model.level
        , history = []
    }


loadNextLevel : Model -> Random Model
loadNextLevel model =
    case Dict.get model.day model.nextEvents of
        Just head ->
            case head of
                WeatherEvent weather ->
                    Puzzle.Setting.toGroups weather.setting
                        |> Random.map
                            (\l ->
                                List.repeat model.money (SingleBlock (OptionalBlock Coin))
                                    ++ (model.items |> List.map (\item -> SingleBlock (OptionalBlock item)))
                                    ++ l
                            )
                        |> Random.andThen
                            (Puzzle.Builder.generateFromGroup (Level.getBlocks model.level))
                        |> Random.andThen
                            (\puzzle ->
                                model
                                    |> clearHistory
                                    |> loadPuzzle puzzle
                            )

        Nothing ->
            model
                |> generateNextMonth
                |> Random.andThen loadNextLevel


applyGenerator : Seed -> Random Model -> Model
applyGenerator seed generator =
    let
        ( model, newSeed ) =
            Random.step generator seed
    in
    { model | seed = newSeed }


performThenShortPauseAndThen : Msg -> Msg -> Msg
performThenShortPauseAndThen msg1 msg2 =
    ApplyThenWait { now = msg1, wait = 100, andThen = msg2 }


performThenPauseAndThen : Msg -> Msg -> Msg
performThenPauseAndThen msg1 msg2 =
    ApplyThenWait { now = msg1, wait = 1000, andThen = msg2 }


generateNextMonth : Model -> Random Model
generateNextMonth model =
    let
        summer =
            not model.summer

        randomSettingsGenerator =
            List.range 1 28
                |> List.foldl
                    (\i ->
                        let
                            difficulty =
                                model.difficutly + toFloat i / 28
                        in
                        Random.andThen
                            (\l ->
                                (Puzzle.Setting.pick
                                    { difficulty = difficulty
                                    , summer = summer
                                    }
                                    (if modBy 7 i == 0 || modBy 7 i == 6 then
                                        Puzzle.Setting.specialSettings

                                     else
                                        Puzzle.Setting.settings
                                    )
                                    |> Random.map
                                        (\setting ->
                                            WeatherEvent
                                                { setting = setting
                                                , reward =
                                                    if i == 28 then
                                                        Coin |> Just

                                                    else
                                                        Nothing
                                                }
                                        )
                                )
                                    |> Random.map (\s -> s :: l)
                            )
                    )
                    (Random.constant [])
    in
    Random.map
        (\randomSettings ->
            { model
                | nextEvents =
                    randomSettings
                        |> List.reverse
                        |> List.indexedMap (\i event -> ( i + 1, event ))
                        |> Dict.fromList
                , summer = summer
                , day = 1
                , year =
                    if not model.summer then
                        model.year + 1

                    else
                        model.year
            }
        )
        randomSettingsGenerator


showCalender : Model -> Model
showCalender model =
    { model | showCalender = True }


closeCalender : Model -> Model
closeCalender model =
    { model | showCalender = False }


increaseDifficulty : Model -> Model
increaseDifficulty model =
    { model
        | difficutly =
            model.difficutly + 1 / 28
    }


addMoney : Model -> Model
addMoney model =
    { model | money = model.money + 1 }


removeMoney : Model -> Model
removeMoney model =
    { model | money = model.money - 1 }


addGroup : Optional -> Model -> Model
addGroup item model =
    { model | items = item :: model.items }



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
            , difficutly = 0
            , day = 0
            , nextEvents =
                [ ( 0, WeatherEvent { setting = Puzzle.Setting.startingLevel, reward = Nothing } ) ]
                    |> Dict.fromList
            , history = []
            , seed = seed
            , summer = False
            , betweenDays = ShowCalenderDay
            , showBetweenDays = False
            , shop = False
            , showCalender = False
            , showPresent = Nothing
            , year = 0
            , money = 0
            , items = []
            }
    in
    ( model
        |> loadNextLevel
        |> applyGenerator seed
    , Random.generate SetSeed Random.independentSeed
    )


addBlock : ( Int, Int ) -> Block -> Model -> Model
addBlock ( x, y ) block model =
    model.level
        |> Level.addBlock ( x, y ) block
        |> (\level -> { model | level = level })


join : ( Int, Int ) -> ( Int, Int ) -> Model -> Maybe Model
join p1 p2 model =
    let
        ( x, y ) =
            Maths.intersect p1 p2
    in
    Maybe.map2
        (\( fruit1, ( _, item1 ) ) ( fruit2, ( _, item2 ) ) ->
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
                        |> (item1
                                |> Maybe.map (Level.addItem ( x, y ))
                                |> Maybe.withDefault identity
                           )
                        |> (item2
                                |> Maybe.map (Level.addItem ( x, y ))
                                |> Maybe.withDefault identity
                           )
                , history = model.level :: model.history
            }
        )
        (model.level |> Level.getEntityAndItem p1)
        (model.level |> Level.getEntityAndItem p2)


endTurn : Model -> ( Model, Cmd Msg )
endTurn model =
    let
        hasWon =
            Dict.toList model.level.fields
                |> List.all
                    (\( _, blockId ) ->
                        model.level.blocks
                            |> Dict.get blockId
                            |> Maybe.map Data.Block.isOptional
                            |> Maybe.withDefault True
                    )
    in
    ( model
    , performThenShortPauseAndThen DoNothing
        (performThenShortPauseAndThen
            (ShowCoins
                [ model.level.nextCoinId - 2
                , model.level.nextCoinId - 1
                ]
            )
            (if hasWon then
                EndDay

             else
                DoNothing
            )
        )
        |> Task.succeed
        |> Task.perform identity
    )


endDay : Model -> ( Model, Cmd Msg )
endDay model =
    let
        showCalenderAndStartNextDay =
            performThenPauseAndThen (SetBetweenDays ShowCalenderDay)
                (performThenPauseAndThen LoadNextLevel
                    StartDay
                )
    in
    case Dict.get model.day model.nextEvents of
        Just (WeatherEvent event) ->
            ( model
                |> setBetweenDays ShowNothing
                |> (event.reward
                        |> Maybe.map (\block -> addGroup block)
                        |> Maybe.withDefault identity
                   )
                |> showBetweenDays
            , (case event.reward of
                Just item ->
                    performThenPauseAndThen DoNothing
                        (performThenPauseAndThen (SetBetweenDays (ShowFoundItem item))
                            (performThenPauseAndThen DoNothing
                                showCalenderAndStartNextDay
                            )
                        )

                Nothing ->
                    showCalenderAndStartNextDay
              )
                |> Task.succeed
                |> Task.perform identity
            )

        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pos ->
            case model.level.selected of
                Nothing ->
                    ( { model | level = Level.setSelected (Just pos) model.level }, Cmd.none )

                Just p ->
                    if p == pos then
                        ( { model | level = Level.setSelected Nothing model.level }, Cmd.none )

                    else if Level.isValidPair pos p model.level then
                        join pos p model
                            |> Maybe.withDefault model
                            |> endTurn

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
                      }
                    , Cmd.none
                    )

                [] ->
                    ( model, Cmd.none )

        ShowCoins list ->
            ( { model
                | level =
                    list
                        |> List.foldl Level.showCoin model.level
              }
            , Cmd.none
            )

        EndDay ->
            endDay model

        SetSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        LoadNextLevel ->
            model
                |> nextDay
                |> loadNextLevel
                |> (\m ->
                        ( m
                            |> applyGenerator model.seed
                        , Cmd.none
                        )
                   )

        NextDay ->
            ( nextDay model, Cmd.none )

        StartDay ->
            ( model
                |> hideBetweenDays
                |> increaseDifficulty
            , Cmd.none
            )

        OpenShop ->
            ( model
                |> openShop
            , Cmd.none
            )

        Buy item ->
            ( model
                |> removeMoney
                |> addGroup item
                |> closeShop
            , Cmd.none
            )

        CloseShop ->
            ( model |> closeShop
            , Cmd.none
            )

        AcceptCoin ->
            ( model
                |> addMoney
            , Task.succeed ()
                |> Task.perform (\() -> EndDay)
            )

        ApplyThenWait args ->
            update args.now model
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , Process.sleep args.wait
                                |> Task.perform (\() -> args.andThen)
                            ]
                    )

        OpenCalender ->
            ( model |> showCalender, Cmd.none )

        CloseCalender ->
            ( model |> closeCalender, Cmd.none )

        SetBetweenDays action ->
            ( model |> setBetweenDays action, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    [ case Dict.get model.day model.nextEvents of
        Just (WeatherEvent weather) ->
            [ View.Header.viewHeader
                { onUndo = Undo
                , onOpenCalender = OpenCalender
                , currentEvent = WeatherEvent weather
                , currentDay = model.day
                }
            , View.Game.viewGame
                { game = model.level
                , onClick = Click
                }
            , [ View.Button.toHtml
                    { label = "Calender"
                    , onPress = OpenCalender
                    }
              ]
                |> Html.div
                    [ Html.Style.flex "1"
                    , Html.Style.displayFlex
                    , Html.Style.justifyContentCenter
                    ]

            {--, Html.text "Click on two different fruits in a row or column to collect them."
                |> List.singleton
                |> Html.div
                    [ Html.Style.padding "8px 16px"
                    , Html.Style.background "white"
                    , Html.Style.borderRadiusPx 32
                    ]--}
            ]
                |> Html.div
                    [ Html.Style.displayFlex
                    , Html.Style.flexDirectionColumn
                    , Html.Style.gapPx 16
                    , Html.Style.widthPx (View.Field.size * 6)
                    , Html.Style.positionRelative
                    ]

        Nothing ->
            Html.text ""
    , Screen.Menu.toHtml
        { show = model.showCalender
        , onClose = CloseCalender
        , today = model.day
        , events = model.nextEvents
        , summer = model.summer
        }
    , case model.betweenDays of
        ShowCalenderDay ->
            Screen.BetweenDays.showCalenderDay
                { nextEvents = model.nextEvents
                , day = model.day
                , show = model.showBetweenDays
                }

        ShowFoundItem item ->
            Screen.BetweenDays.showFoundItem
                { item = item
                , show = model.showBetweenDays
                }

        ShowNothing ->
            Screen.BetweenDays.showNothing { show = model.showBetweenDays }
    , Stylesheet.stylesheet
    , Html.node "meta"
        [ Html.Attributes.name "viewport"
        , Html.Attributes.attribute "content" "width=400, initial-scale=1,user-scalable=no"
        ]
        []
    ]
        |> (if model.summer then
                View.Background.summerGrass

            else
                View.Background.summerGrass
            --View.Background.winterGrass
           )
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.alignItemsCenter
            , Html.Style.justifyContentCenter
            , Html.Style.positionRelative
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
