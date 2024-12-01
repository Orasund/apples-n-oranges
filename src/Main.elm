module Main exposing (main)

import Bag exposing (Bag, Item(..))
import Browser
import Data.Block exposing (Block(..))
import Dict
import Event exposing (Event(..))
import Html exposing (Html)
import Html.Attributes
import Html.Style
import Level exposing (CoinId, Level, Puzzle)
import Maths
import Process
import Puzzle.Builder
import Puzzle.Setting exposing (Setting)
import Random exposing (Generator, Seed)
import Stylesheet
import Task
import View.Background
import View.Calender
import View.EndOfDay
import View.Field
import View.Game
import View.Header
import View.Shop


type alias Random a =
    Generator a


type alias Model =
    { level : Level
    , difficutly : Float
    , bag : Bag
    , day : Int
    , event : Event
    , nextEvents : List Event
    , seed : Seed
    , history : List Level
    , possibleSettings : List Setting
    , endOfDay : Bool
    , summer : Bool
    , shop :
        Maybe
            { buyableSettings : List Setting
            , selected : Maybe Int
            }
    , showCalender : Bool
    }


type Msg
    = Click ( Int, Int )
    | ShowCoins (List CoinId)
    | CollectCoin
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
    | OpenCalender
    | CloseCalender


collectCoins : Model -> Model
collectCoins model =
    let
        ( level, amount ) =
            model.level
                |> Level.collectCoins
    in
    { model
        | bag =
            amount
                |> List.foldl Bag.insert
                    model.bag
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
    let
        summer =
            if model.day + 1 == 28 then
                not model.summer

            else
                model.summer
    in
    Random.map2
        (\buyableSettings possibleSettings ->
            { model
                | possibleSettings = possibleSettings
                , shop =
                    Just
                        { buyableSettings = buyableSettings
                        , selected = Nothing
                        }
            }
        )
        (Random.list 2
            (Puzzle.Setting.pickSettings
                { difficulty = model.difficutly
                , summer = summer
                }
            )
        )
        (Random.list 6
            (Puzzle.Setting.pickSettings
                { difficulty = model.difficutly
                , summer = summer
                }
            )
        )


closeShop : Model -> Model
closeShop model =
    { model | shop = Nothing }


nextDay : Model -> Model
nextDay model =
    let
        day =
            model.day + 1
    in
    { model
        | day = day |> modBy 28
        , summer =
            if day >= 28 then
                not model.summer

            else
                model.summer
    }


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
    case model.nextEvents of
        head :: tail ->
            case head of
                WeatherEvent weather ->
                    weather
                        |> Puzzle.Setting.toGroups
                        |> Puzzle.Builder.generateFromGroup (Level.getBlocks model.level)
                        |> Random.andThen
                            (\puzzle ->
                                { model
                                    | event = head
                                    , nextEvents = tail
                                }
                                    |> clearHistory
                                    |> loadPuzzle puzzle
                            )

                ShopEvent ->
                    { model
                        | event = head
                        , nextEvents = tail
                    }
                        -- |> clearHistory
                        |> openShop

        [] ->
            Random.constant model


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
                }
            )
        |> Maybe.withDefault model


generateNextWeek : Model -> Model
generateNextWeek model =
    { model
        | nextEvents =
            List.map WeatherEvent model.possibleSettings
                ++ [ ShopEvent ]
    }


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
            , bag = Bag.empty
            , day = 1
            , event = WeatherEvent Puzzle.Setting.startingLevel
            , nextEvents =
                List.map WeatherEvent Puzzle.Setting.tutorials
                    ++ [ ShopEvent ]
            , history = []
            , possibleSettings = Puzzle.Setting.tutorials
            , seed = seed
            , summer = True
            , endOfDay = False
            , shop = Nothing
            , showCalender = False
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


checkWinCondition : Model -> ( Model, Cmd Msg )
checkWinCondition model =
    let
        hasWon =
            Dict.toList model.level.fields
                |> List.all
                    (\( _, blockId ) ->
                        case model.level.blocks |> Dict.get blockId of
                            Just (OrganicBlock _) ->
                                False

                            Just FishingRod ->
                                False

                            Just Dynamite ->
                                False

                            Just (OptionalBlock _) ->
                                True

                            Nothing ->
                                True
                    )
    in
    ( model
    , shortWaitThenPerform
        (ShowCoins
            [ model.level.nextCoinId - 2
            , model.level.nextCoinId - 1
            ]
        )
        :: (if hasWon then
                [ longWaitThenPerform CollectCoin
                , shortWaitThenPerform EndDay
                , longWaitThenPerform LoadNextLevel
                ]

            else
                []
           )
        |> sequence
    )


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
                      }
                    , Cmd.none
                    )

                [] ->
                    ( model, Cmd.none )

        CollectCoin ->
            ( model |> collectCoins, Cmd.none )

        ShowCoins list ->
            ( { model
                | level =
                    list
                        |> List.foldl Level.showCoin model.level
              }
            , Cmd.none
            )

        EndDay ->
            ( model
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
            ( model
                |> gotoLevel
                |> increaseDifficulty
            , Cmd.none
            )

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

        OpenCalender ->
            ( model |> showCalender, Cmd.none )

        CloseCalender ->
            ( model |> closeCalender, Cmd.none )


view : Model -> Html Msg
view model =
    [ case model.shop of
        Nothing ->
            [ View.Header.viewHeader
                { onUndo = Undo
                , onOpenCalender = OpenCalender
                , currentEvent = model.event
                , currentDay = model.day
                }
            , View.Game.viewGame
                { game = model.level
                , onClick = Click
                }
            , Html.text "Click on two different fruits in a row or column to collect them."
                |> List.singleton
                |> Html.div
                    [ Html.Style.padding "8px 16px"
                    , Html.Style.background "white"
                    , Html.Style.borderRadiusPx 32
                    ]
            ]
                |> Html.div
                    [ Html.Style.displayFlex
                    , Html.Style.flexDirectionColumn
                    , Html.Style.gapPx 16
                    , Html.Style.widthPx (View.Field.size * 6)
                    ]

        Just shop ->
            View.Shop.toHtml
                { settings = model.possibleSettings
                , buyableSettings = shop.buyableSettings
                , selected = shop.selected
                , onSelectSettingToBuy = SelectSettingToBuy
                , onBuy = BuySettingAndReplaceWith
                }
    , View.Calender.toHtml
        { show = model.showCalender
        , onClose = CloseCalender
        , today = model.day
        , summer = model.summer
        }
    , View.EndOfDay.toHtml
        { currentEvent = model.event
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
        |> (if model.summer then
                View.Background.summerGrass

            else
                View.Background.winterGrass
           )
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
