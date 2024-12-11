module Main exposing (main)

import Browser
import Data.Block exposing (Block(..), Item(..), Optional(..))
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Style
import ItemBag exposing (ItemBag)
import Level exposing (CoinId, Level, Puzzle)
import Maths
import Process
import Puzzle.Builder exposing (Group(..))
import Puzzle.Setting exposing (Event)
import Random exposing (Generator, Seed)
import Screen.BetweenDays exposing (BetweenDaysAction(..))
import Screen.Menu exposing (MenuTab(..), Trade)
import Set
import Stylesheet
import Task
import View.Background
import View.Button
import View.Field
import View.Level


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
    , menu : MenuTab
    , trades : List Trade
    , showMenu : Bool
    , summer : Bool
    , shop : Bool
    , year : Int
    , items : ItemBag
    , pointerZero : ( Float, Float )
    , pointer : Maybe ( Float, Float )
    }


type Msg
    = PointerDown { pos : ( Float, Float ), offset : ( Float, Float ) }
    | PointerUp ( Float, Float )
    | PointerEnd ( Float, Float )
    | ShowCoins (List CoinId)
    | Undo
    | EndDay
    | SetSeed Seed
    | NextDay
    | LoadNextLevel
    | SetBetweenDays BetweenDaysAction
    | StartDay
    | OpenShop
    | AcceptTrade Trade
    | CloseShop
    | ApplyThenWait { now : Msg, wait : Float, andThen : Msg }
    | OpenCalender
    | CloseCalender
    | DoNothing
    | SetMenuTab MenuTab


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
        ({ model
            | items =
                ItemBag.toList model.items
                    |> List.foldl
                        (\( item, positions ) args ->
                            let
                                n =
                                    Set.size positions
                            in
                            { positions = List.drop n args.positions
                            , out = ( item, List.take n args.positions ) :: args.out
                            }
                        )
                        { positions = Set.toList puzzle.solids
                        , out = []
                        }
                    |> .out
                    |> ItemBag.fromList
         }
            |> Random.constant
        )
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
        Just event ->
            Puzzle.Builder.generate
                { pairs =
                    Puzzle.Setting.toList event.setting
                        |> List.map (\( a, b ) -> Pair a b)
                , solids =
                    model.items
                        |> ItemBag.toList
                        |> List.map (\( _, set ) -> Set.size set)
                        |> List.sum
                }
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
    { model | showMenu = True }


closeCalender : Model -> Model
closeCalender model =
    { model | showMenu = False }


increaseDifficulty : Model -> Model
increaseDifficulty model =
    { model
        | difficutly =
            model.difficutly + 1 / 28
    }


addItem : Item -> Model -> Model
addItem item model =
    { model | items = model.items |> ItemBag.insert ( -1, -1 ) item }


removeItem : Item -> Model -> Model
removeItem item model =
    { model
        | items =
            model.items
                |> ItemBag.remove item
                |> Maybe.map Tuple.first
                |> Maybe.withDefault model.items
    }


replaceItem : { from : Item, to : Item } -> Model -> Model
replaceItem args model =
    { model
        | items =
            model.items
                |> ItemBag.remove args.from
                |> Maybe.map
                    (\( items, pos ) ->
                        items |> ItemBag.insert pos args.to
                    )
                |> Maybe.withDefault model.items
    }


tradeItems : { remove : List ( Item, Int ), add : List Item } -> Model -> Model
tradeItems args model =
    args.remove
        |> List.concatMap (\( item, n ) -> List.repeat n item)
        |> List.foldl
            (\item { add, out } ->
                case add of
                    head :: tail ->
                        { add = tail
                        , out = replaceItem { from = item, to = head } out
                        }

                    [] ->
                        { add = []
                        , out = removeItem item out
                        }
            )
            { add = args.add
            , out = model
            }
        |> .out


setMenuTab : MenuTab -> Model -> Model
setMenuTab menuTab model =
    { model | menu = menuTab }



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
                [ ( 0, { setting = Puzzle.Setting.startingLevel, reward = Nothing } ) ]
                    |> Dict.fromList
            , history = []
            , seed = seed
            , summer = False
            , betweenDays = ShowCalenderDay
            , showBetweenDays = False
            , menu = CalenderTab
            , showMenu = False
            , shop = False
            , year = 0
            , trades =
                [ { remove = [ ( Coin, 2 ) ]
                  , add = BagOfCoins
                  , trader = "👩🏻 Alice"
                  }
                , { remove = [ ( BagOfCoins, 2 ) ]
                  , add = Diamand
                  , trader = "👨🏼 Rick"
                  }
                ]
            , items = ItemBag.empty
            , pointerZero = ( 0, 0 )
            , pointer = Nothing
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
                                |> Maybe.map (Level.addPartOfPair ( x, y ))
                                |> Maybe.withDefault identity
                           )
                        |> (item2
                                |> Maybe.map (Level.addPartOfPair ( x, y ))
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
        Just event ->
            ( model
                |> setBetweenDays ShowNothing
                |> (event.reward
                        |> Maybe.map (\block -> addItem block)
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


click : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
click pos model =
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


swipe : { from : ( Float, Float ), to : ( Float, Float ) } -> Model -> Maybe ( Int, Int )
swipe args model =
    let
        ( fromX, fromY ) =
            args.from

        ( toX, toY ) =
            args.to

        rec ( x, y ) ( stepX, stepY ) =
            if x < 0 || y < 0 || x >= 6 || y >= 6 then
                Nothing

            else
                case Level.getBlockAt ( x + stepX |> floor, y + stepY |> floor ) model.level of
                    Just _ ->
                        ( x + stepX |> floor, y + stepY |> floor )
                            |> Just

                    Nothing ->
                        rec ( x + stepX, y + stepY ) ( stepX, stepY )
    in
    if abs (fromX - toX) > abs (fromY - toY) then
        rec args.from
            (if (toX - fromX) < 0 then
                ( -1, 0 )

             else
                ( 1, 0 )
            )

    else
        rec args.from
            (if (toY - fromY) < 0 then
                ( 0, -1 )

             else
                ( 0, 1 )
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PointerDown pointer ->
            let
                ( x, y ) =
                    pointer.pos
            in
            click ( floor x, floor y )
                { model
                    | pointerZero = pointer.offset
                    , pointer = Just pointer.pos
                }

        PointerUp to ->
            model.pointer
                |> Maybe.andThen
                    (\( x, y ) ->
                        swipe
                            { from = ( x, y )
                            , to = to
                            }
                            model
                            |> Maybe.map
                                (\pos ->
                                    click
                                        pos
                                        { model | pointer = Nothing }
                                )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        PointerEnd to ->
            model.pointer
                |> Maybe.map
                    (\( x, y ) ->
                        click
                            (swipe
                                { from = ( x, y )
                                , to = to
                                }
                                model
                                |> Maybe.withDefault ( floor x, floor y )
                            )
                            { model | pointer = Nothing }
                    )
                |> Maybe.withDefault ( model, Cmd.none )

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

        AcceptTrade args ->
            ( model
                |> tradeItems
                    { remove = args.remove
                    , add = [ args.add ]
                    }
            , Cmd.none
            )

        CloseShop ->
            ( model |> closeShop
            , Cmd.none
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

        SetMenuTab menu ->
            ( model |> setMenuTab menu, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    [ [ Html.div
            [ Html.Style.displayFlex
            , Html.Style.alignItemsEnd
            , Html.Style.justifyContentCenter
            , Html.Style.gapPx 4
            , Html.Style.width "100%"
            ]
            [ View.Button.toHtml []
                { label = "Menu"
                , onPress = OpenCalender
                }
                |> List.singleton
                |> Html.div
                    [ Html.Style.flex "1"
                    , Html.Style.displayFlex
                    ]
            ]
      , View.Level.viewGame
            { game = model.level
            , items = model.items
            , onPointerDown = PointerDown
            , onPointerUp = PointerUp
            , onPointerEnd = PointerEnd
            , zero = model.pointerZero
            }
      , View.Button.toHtml []
            { label = "Undo"
            , onPress = Undo
            }
            |> List.singleton
            |> Html.div
                [ Html.Style.flex "1"
                , Html.Style.displayFlex
                , Html.Style.justifyContentCenter
                ]
      ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.gapPx 16
            , Html.Style.widthPx (View.Field.size * 6)
            , Html.Style.positionRelative
            ]
    , case model.menu of
        CalenderTab ->
            Screen.Menu.calender
                { show = model.showMenu
                , today = model.day
                , events = model.nextEvents
                , summer = model.summer
                , onSelectTab = SetMenuTab
                , onClose = CloseCalender
                }

        MarketTab ->
            Screen.Menu.market
                { show = model.showMenu
                , trades = model.trades
                , items = model.items
                , onAcceptTrade = AcceptTrade
                , onSelectTab = SetMenuTab
                , onClose = CloseCalender
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
        |> View.Background.summerGrass
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
