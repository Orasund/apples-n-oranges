module Main exposing (main)

import Browser
import Data.Block exposing (Block(..), Item(..), Optional(..))
import Data.Date as Date exposing (Date)
import Data.ItemBag exposing (ItemBag)
import Data.Message exposing (Mail)
import Data.Person exposing (Person)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Style
import Level exposing (CoinId, Level, Puzzle)
import Maths
import Process
import Puzzle.Builder exposing (Group(..))
import Puzzle.Setting exposing (Event)
import Random exposing (Generator, Seed)
import Screen.BetweenDays exposing (BetweenDaysAction(..))
import Screen.Game
import Screen.Menu exposing (MenuTab(..), Trade)
import Set
import Stylesheet
import Task
import View.Background


type alias Random a =
    Generator a


type alias Model =
    { level : Level
    , difficutly : Float
    , date : Date
    , nextEvents : Dict Date Event
    , seed : Seed
    , history : List Level
    , betweenDaysLast : BetweenDaysAction
    , betweenDays : List BetweenDaysAction
    , showBetweenDays : Bool
    , menu : MenuTab
    , trades : List Trade
    , messages : Dict Date Mail
    , nextMessages : Dict String Mail
    , showMenu : Bool
    , shop : Bool
    , items : ItemBag
    , peoples : Dict Int Person
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
    | LoadNextLevel
    | SetBetweenDays BetweenDaysAction
    | StartDay
    | OpenShop
    | AcceptTrade Trade
    | CloseShop
    | OpenCalender
    | CloseCalender
    | DoNothing
    | SetMenuTab MenuTab
    | AcceptMail Date
    | NextActionBetweenDays


addBetweenDaysActions : List BetweenDaysAction -> Model -> Model
addBetweenDaysActions list model =
    { model | betweenDays = model.betweenDays ++ list }


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
    { model | date = Date.next model.date }


loadPuzzle : Puzzle -> Model -> Random Model
loadPuzzle puzzle model =
    List.foldl
        (\( pos, fruit ) ->
            Random.map (Level.addBlock pos fruit)
        )
        (model.level
            |> Level.clear
            |> Level.setSolids puzzle.solids
            |> Random.constant
        )
        puzzle.blocks
        |> Random.map
            (\level ->
                { model
                    | level = level
                    , history = []
                    , items =
                        Data.ItemBag.toList model.items
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
                            |> Data.ItemBag.fromList
                }
            )


loadNextLevel : Model -> Random Model
loadNextLevel model =
    case Dict.get model.date model.nextEvents of
        Just event ->
            Puzzle.Builder.generate
                { pairs =
                    Puzzle.Setting.toList event.setting
                        |> List.map (\( a, b ) -> Pair a b)
                , solids =
                    model.items
                        |> Data.ItemBag.toList
                        |> List.map (\( _, set ) -> Set.size set)
                        |> List.sum
                }
                |> Random.andThen
                    (\puzzle ->
                        model
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


generateNextMonth : Model -> Random Model
generateNextMonth model =
    let
        randomSettingsGenerator =
            Date.listOfDaysInMonth model.date
                |> List.foldl
                    (\date ->
                        let
                            i =
                                Date.day date

                            difficulty =
                                model.difficutly + toFloat i / Date.daysInAMonth
                        in
                        Random.andThen
                            (\l ->
                                Puzzle.Setting.pick
                                    { difficulty = difficulty
                                    , summer = Date.summer date
                                    }
                                    (if modBy 7 i == 0 || modBy 7 i == 6 then
                                        Puzzle.Setting.specialSettings

                                     else
                                        Puzzle.Setting.settings
                                    )
                                    |> Random.map2
                                        (\rand01 setting ->
                                            ( date
                                            , { setting = setting
                                              , reward = (rand01 == 0) && (modBy 7 i == 0 || modBy 7 i == 6)
                                              , mail = modBy 5 i == 0
                                              }
                                            )
                                        )
                                        (Random.int 0 3)
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
                        |> Dict.fromList
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
            model.difficutly + 1 / Date.daysInAMonth
    }


addItem : Item -> Model -> Model
addItem item model =
    { model | items = model.items |> Data.ItemBag.insert ( -1, -1 ) item }


removeItem : Item -> Model -> Model
removeItem item model =
    { model
        | items =
            model.items
                |> Data.ItemBag.remove item
                |> Maybe.map Tuple.first
                |> Maybe.withDefault model.items
    }


replaceItem : { from : Item, to : Item } -> Model -> Model
replaceItem args model =
    { model
        | items =
            model.items
                |> Data.ItemBag.remove args.from
                |> Maybe.map
                    (\( items, pos ) ->
                        items |> Data.ItemBag.insert pos args.to
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


acceptMail : Date -> Model -> Random Model
acceptMail n model =
    model.messages
        |> Dict.get n
        |> Maybe.map
            (\mail ->
                let
                    sender =
                        mail.sender |> (\s -> { s | progress = s.progress + 1 })
                in
                Data.Message.next sender
                    |> Maybe.map Random.constant
                    |> Maybe.withDefault (Data.Message.default sender)
                    |> Random.map
                        (\nextMessage ->
                            { model
                                | messages =
                                    model.messages
                                        |> Dict.insert n
                                            { mail | accepted = True }
                                , nextMessages =
                                    model.nextMessages
                                        |> Dict.insert (Data.Person.jobToString mail.sender.job)
                                            nextMessage
                            }
                        )
            )
        |> Maybe.withDefault (Random.constant model)


addMail : Model -> Generator Model
addMail model =
    model.nextMessages
        |> Dict.values
        |> Maths.shuffle
        |> Random.map List.head
        |> Random.andThen
            (\maybe ->
                maybe
                    |> Maybe.map
                        (\mail ->
                            Data.Message.default mail.sender
                                |> Random.map
                                    (\nextMessage ->
                                        { model
                                            | messages = Dict.insert model.date mail model.messages
                                            , nextMessages =
                                                model.nextMessages
                                                    |> Dict.insert (Data.Person.jobToString mail.sender.job)
                                                        nextMessage
                                        }
                                    )
                        )
                    |> Maybe.withDefault (Random.constant model)
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

        people =
            [ Data.Person.alice
            , Data.Person.rick
            ]

        model : Model
        model =
            { level = Level.empty { columns = 6, rows = 6 }
            , difficutly = 0
            , date = Date.zero
            , nextEvents =
                [ ( Date.zero
                  , { setting = Puzzle.Setting.startingLevel
                    , reward = False
                    , mail = False
                    }
                  )
                ]
                    |> Dict.fromList
            , history = []
            , seed = seed
            , betweenDaysLast = ShowNothing
            , betweenDays = []
            , showBetweenDays = False
            , menu = MailTab
            , messages = Dict.empty
            , nextMessages =
                people
                    |> List.filterMap
                        (\person ->
                            Data.Message.next person
                                |> Maybe.map (Tuple.pair (Data.Person.jobToString person.job))
                        )
                    |> Dict.fromList
            , showMenu = False
            , shop = False
            , trades =
                [ { remove = [ ( Coin, 2 ) ]
                  , add = BagOfCoins
                  , trader = Data.Person.alice.name
                  }
                , { remove = [ ( BagOfCoins, 2 ) ]
                  , add = Diamand
                  , trader = Data.Person.rick.name
                  }
                ]
            , items = Data.ItemBag.empty
            , pointerZero = ( 0, 0 )
            , pointer = Nothing
            , peoples =
                people
                    |> List.indexedMap Tuple.pair
                    |> Dict.fromList
            }
    in
    ( model
        |> loadNextLevel
        |> applyGenerator seed
    , Random.generate SetSeed Random.independentSeed
    )


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
    , [ Process.sleep
            100
            |> Task.perform
                (\() ->
                    ShowCoins
                        [ model.level.nextCoinId - 2
                        , model.level.nextCoinId - 1
                        ]
                )
      , Process.sleep
            500
            |> Task.perform
                (\() ->
                    if hasWon then
                        EndDay

                    else
                        DoNothing
                )
      ]
        |> Cmd.batch
    )


endDay : Model -> Model
endDay model =
    case Dict.get model.date model.nextEvents of
        Just event ->
            model
                |> addBetweenDaysActions
                    ([ [ ShowNothing ]
                     , if event.reward then
                        [ ShowItemAdded event.setting.reward
                        , ShowNothing
                        ]

                       else
                        []
                     , if event.mail then
                        [ ShowMail, ShowNothing ]

                       else
                        []
                     , [ --ShowCalenderDay
                         --,
                         AdvanceCalenderDay
                       , ShowCalenderDay
                       ]
                     ]
                        |> List.concat
                    )
                |> showBetweenDays

        Nothing ->
            model


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
    if max (abs (fromX - toX)) (abs (fromY - toY)) < 0.25 then
        Nothing

    else if abs (fromX - toX) > abs (fromY - toY) then
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


applyAction : BetweenDaysAction -> Model -> Model
applyAction action model =
    case action of
        ShowItemAdded item ->
            addItem item model

        ShowItemRemoved item ->
            removeItem item model

        AdvanceCalenderDay ->
            model |> nextDay

        ShowMail ->
            model
                |> addMail
                |> applyGenerator model.seed

        ShowCalenderDay ->
            model

        ShowNothing ->
            model


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
            ( model |> endDay
            , NextActionBetweenDays
                |> Task.succeed
                |> Task.perform identity
            )

        SetSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        LoadNextLevel ->
            ( model
                |> loadNextLevel
                |> applyGenerator model.seed
            , Cmd.none
            )

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

        OpenCalender ->
            ( model |> showCalender, Cmd.none )

        CloseCalender ->
            ( model |> closeCalender, Cmd.none )

        SetBetweenDays action ->
            ( model |> addBetweenDaysActions [ action ], Cmd.none )

        SetMenuTab menu ->
            ( model |> setMenuTab menu, Cmd.none )

        AcceptMail i ->
            ( case Dict.get i model.messages of
                Just mail ->
                    mail.request
                        |> Maybe.map
                            (\item ->
                                { model
                                    | items =
                                        model.items
                                            |> Data.ItemBag.remove item
                                            |> Maybe.map Tuple.first
                                            |> Maybe.withDefault model.items
                                }
                            )
                        |> Maybe.withDefault model
                        |> addBetweenDaysActions
                            ([ mail.present
                                |> Maybe.map
                                    (\item ->
                                        [ ShowItemAdded item
                                        , ShowNothing
                                        ]
                                    )
                                |> Maybe.withDefault []
                             ]
                                |> List.concat
                            )
                        |> acceptMail i
                        |> applyGenerator model.seed

                Nothing ->
                    model
            , Cmd.none
            )

        DoNothing ->
            ( model, Cmd.none )

        NextActionBetweenDays ->
            case model.betweenDays of
                [ _ ] ->
                    ( { model | showBetweenDays = False }
                        |> loadNextLevel
                        |> applyGenerator model.seed
                    , Process.sleep 1000 |> Task.perform (\() -> StartDay)
                    )

                head :: tail ->
                    ( { model
                        | betweenDaysLast = head
                        , betweenDays = tail
                      }
                        |> applyAction head
                    , Process.sleep 800 |> Task.perform (\() -> NextActionBetweenDays)
                    )

                [] ->
                    ( model, Cmd.none )


viewBetweenDays : BetweenDaysAction -> Model -> Html Msg
viewBetweenDays action model =
    case action of
        ShowCalenderDay ->
            Screen.BetweenDays.showCalenderDay
                { nextEvents = model.nextEvents
                , date = model.date
                }

        AdvanceCalenderDay ->
            Screen.BetweenDays.showCalenderDay
                { nextEvents = model.nextEvents
                , date = model.date
                }

        ShowItemAdded item ->
            Screen.BetweenDays.showItemAdded
                { item = item
                }

        ShowItemRemoved item ->
            Screen.BetweenDays.showItemRemoved
                { item = item
                }

        ShowMail ->
            Screen.BetweenDays.showMail

        ShowNothing ->
            Screen.BetweenDays.showNothing


view : Model -> Html Msg
view model =
    [ Screen.Game.toHtml
        { items = model.items
        , level = model.level
        , showDot =
            model.messages
                |> Dict.toList
                |> List.filter (\( _, mail ) -> not mail.accepted)
                |> List.length
        , pointerZero = model.pointerZero
        , onOpenMenu = OpenCalender
        , onPointerDown = PointerDown
        , onPointerEnd = PointerEnd
        , onPointerUp = PointerUp
        , onUndo = Undo
        }
    , case model.menu of
        CalenderTab ->
            Screen.Menu.calender
                { show = model.showMenu
                , date = model.date
                , events = model.nextEvents
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

        MailTab ->
            Screen.Menu.messages
                { show = model.showMenu
                , mails = model.messages
                , items = model.items
                , onAccept = AcceptMail
                , onSelectTab = SetMenuTab
                , onClose = CloseCalender
                }
    , viewBetweenDays
        (model.betweenDays
            |> List.head
            |> Maybe.withDefault ShowNothing
        )
        model
        |> List.singleton
        |> Screen.BetweenDays.toHtml { show = model.showBetweenDays }
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
