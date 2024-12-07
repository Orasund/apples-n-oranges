module Main exposing (main)

import Bag exposing (Bag, Item(..))
import Browser
import Data.Artefact exposing (Artifact)
import Data.Block exposing (Block(..))
import Dict exposing (Dict)
import Event exposing (Event(..))
import Html exposing (Html)
import Html.Attributes
import Html.Style
import Level exposing (CoinId, Level, Puzzle)
import Maths
import Process
import Puzzle.Builder
import Puzzle.Setting
import Random exposing (Generator, Seed)
import Screen.EndOfDay
import Screen.Shop
import Set exposing (Set)
import Stylesheet
import Task
import View.Background
import View.Calender
import View.Field
import View.Game
import View.Header


type alias Random a =
    Generator a


type alias Model =
    { level : Level
    , difficutly : Float
    , bag : Bag
    , day : Int
    , nextEvents : Dict Int Event
    , seed : Seed
    , history : List Level
    , endOfDay : Bool
    , summer : Bool
    , shop : Bool
    , showCalender : Bool
    , showPresent : Maybe Artifact
    , artifacts : Set String
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
    | CloseShop
    | Wait Float Msg
    | Sequence (List Msg)
    | OpenCalender
    | CloseCalender
    | ShowArtifaact
    | CloseArtifact


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
                    weather.setting
                        |> Puzzle.Setting.toGroups
                        |> Puzzle.Builder.generateFromGroup (Level.getBlocks model.level)
                        |> Random.andThen
                            (\puzzle ->
                                model
                                    |> clearHistory
                                    |> loadPuzzle puzzle
                            )

                ShopEvent ->
                    model
                        |> openShop
                        |> Random.constant

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
                                Puzzle.Setting.pick
                                    { difficulty = difficulty
                                    , summer = summer
                                    }
                                    (if modBy 7 i == 0 || modBy 7 i == 6 then
                                        Puzzle.Setting.specialSettings

                                     else
                                        Puzzle.Setting.settings
                                    )
                                    |> Random.map (\s -> s :: l)
                            )
                    )
                    (Random.constant [])
    in
    Random.map2
        (\randomSettings present ->
            { model
                | nextEvents =
                    randomSettings
                        |> List.reverse
                        |> List.indexedMap
                            (\i setting ->
                                ( i + 1
                                , WeatherEvent
                                    { setting = setting
                                    , present = present
                                    }
                                )
                            )
                        |> Dict.fromList
                , summer = summer
                , day = 1
            }
        )
        randomSettingsGenerator
        (Random.int 0 1 |> Random.map ((==) 0))


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


addPresent : Model -> Random Model
addPresent model =
    case Data.Artefact.list of
        head :: tail ->
            Random.uniform head tail
                |> Random.map
                    (\artficat ->
                        { model | artifacts = model.artifacts |> Set.insert (Data.Artefact.toString artficat) }
                    )

        [] ->
            Random.constant model


clearPresent : Model -> Model
clearPresent model =
    { model | showPresent = Nothing }



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
            , day = 0
            , nextEvents =
                [ ( 0, WeatherEvent { setting = Puzzle.Setting.startingLevel, present = False } ) ]
                    |> Dict.fromList
            , history = []
            , seed = seed
            , summer = False
            , endOfDay = False
            , shop = False
            , showCalender = False
            , showPresent = Nothing
            , artifacts = Set.empty
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
    , shortWaitThenPerform
        (ShowCoins
            [ model.level.nextCoinId - 2
            , model.level.nextCoinId - 1
            ]
        )
        :: (if hasWon then
                [ [ longWaitThenPerform CollectCoin ]
                , case Dict.get model.day model.nextEvents of
                    Just (WeatherEvent { present }) ->
                        if present then
                            [ ShowArtifaact ]

                        else
                            []

                    _ ->
                        []
                , [ shortWaitThenPerform EndDay
                  , longWaitThenPerform LoadNextLevel
                  ]
                ]
                    |> List.concat

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
                |> nextDay
                |> loadNextLevel
                |> (\m ->
                        ( m
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
            , Cmd.none
            )

        CloseShop ->
            ( model |> closeShop
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

        ShowArtifaact ->
            ( model
                |> addPresent
                |> applyGenerator model.seed
            , Cmd.none
            )

        CloseArtifact ->
            ( model |> clearPresent, Cmd.none )


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

        _ ->
            Screen.Shop.toHtml
                { onClose = CloseShop
                }
    , View.Calender.toHtml
        { show = model.showCalender
        , onClose = CloseCalender
        , today = model.day
        , events = model.nextEvents
        , summer = model.summer
        }
    , Screen.EndOfDay.toHtml
        { nextEvents = model.nextEvents
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
