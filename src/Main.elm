module Main exposing (main)

import Browser
import Data.Block exposing (Block(..), Item(..))
import Data.Date as Date exposing (Date)
import Data.ItemBag exposing (ItemBag)
import Data.Person exposing (Message, Person, PersonId)
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
import Screen.Menu exposing (Filter, MenuTab(..), Trade)
import Set exposing (Set)
import Stylesheet
import Task
import View.Background


type alias Random a =
    Generator a


type alias MessageId =
    Date


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
    , menu : Maybe MenuTab
    , trades : List Trade
    , showMenu : Bool
    , items : ItemBag
    , answeredMessages : Set MessageId
    , messages :
        Dict
            MessageId
            { personId : PersonId
            , message : Message
            }
    , persons : Dict PersonId Person
    , filterMessages : Maybe Filter
    , pointerZero : ( Float, Float )
    , pointer : Maybe ( Float, Float )
    , stopGame : Bool
    }


type Msg
    = PointerDown { pos : ( Float, Float ), offset : ( Float, Float ) }
    | PointerUp ( Float, Float )
    | PointerEnd ( Float, Float )
    | ShowCoins (List CoinId)
    | Undo
    | Reset
    | EndDay
    | SetSeed Seed
    | LoadNextLevel
    | SetBetweenDays BetweenDaysAction
    | StartDay
    | AcceptTrade Trade
    | OpenMenu MenuTab
    | CloseCalender
    | DoNothing
    | SetMenuTab MenuTab
    | AcceptMail Date
    | NextActionBetweenDays
    | SetFilterMessages (Maybe Filter)
    | ContinueGame


addBetweenDaysActions : List BetweenDaysAction -> Model -> Model
addBetweenDaysActions list model =
    { model | betweenDays = model.betweenDays ++ list }


showBetweenDays : Model -> Model
showBetweenDays model =
    { model | showBetweenDays = True }


hideBetweenDays : Model -> Model
hideBetweenDays model =
    { model | showBetweenDays = False }


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
    Puzzle.Setting.generateMonth
        { date = model.date
        , difficutly = model.difficutly
        }
        |> Random.map
            (\randomSettings ->
                { model
                    | nextEvents =
                        randomSettings
                            |> List.reverse
                            |> Dict.fromList
                }
            )


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


setMenuTab : Maybe MenuTab -> Model -> Model
setMenuTab menuTab model =
    { model | menu = menuTab }


acceptMail : Date -> Model -> Random Model
acceptMail date model =
    let
        fun : { personId : Int, person : Person, mail : Message } -> Random Model
        fun { personId, person, mail } =
            Data.Person.next
                { job = person.job
                , progress = person.progress
                }
                |> Maybe.map Random.constant
                |> Maybe.withDefault Data.Person.default
                |> Random.map
                    (\nextMessage ->
                        { model
                            | messages =
                                model.messages
                                    |> Dict.insert date
                                        { message = mail
                                        , personId = personId
                                        }
                            , persons =
                                model.persons
                                    |> Dict.insert personId
                                        (person
                                            |> Data.Person.setNextMessage nextMessage
                                            |> Data.Person.increaseFriendship
                                        )
                            , answeredMessages =
                                model.answeredMessages
                                    |> Set.insert date
                        }
                            |> addBetweenDaysActions
                                (if (Data.Person.increaseFriendship person).friendship == Data.Person.friendshipForLove then
                                    [ ShowLover (Data.Person.increaseFriendship person)
                                    , ShowLover (Data.Person.increaseFriendship person)
                                    ]

                                 else if (Data.Person.increaseFriendship person).friendship == Data.Person.friendshipForLove // 2 then
                                    [ ShowFriendship (Data.Person.increaseFriendship person)
                                    , ShowFriendship (Data.Person.increaseFriendship person)
                                    ]

                                 else
                                    []
                                )
                    )
    in
    model.messages
        |> Dict.get date
        |> Maybe.andThen
            (\{ personId, message } ->
                model.persons
                    |> Dict.get personId
                    |> Maybe.map
                        (\person ->
                            fun
                                { personId = personId
                                , person = Data.Person.advanceProgress person
                                , mail = message
                                }
                        )
            )
        |> Maybe.withDefault (Random.constant model)


addMail : Model -> Generator Model
addMail model =
    let
        fun : { personId : PersonId, person : Person } -> Random Model
        fun args =
            Data.Person.default
                |> Random.map
                    (\newMessage ->
                        { model
                            | messages =
                                Dict.insert model.date
                                    { personId = args.personId
                                    , message = args.person.nextMessage
                                    }
                                    model.messages
                            , persons =
                                model.persons
                                    |> Dict.insert args.personId
                                        (Data.Person.setNextMessage newMessage args.person)
                        }
                    )
    in
    model.persons
        |> Dict.toList
        |> Maths.shuffle
        |> Random.map List.head
        |> Random.andThen
            (\maybe ->
                maybe
                    |> Maybe.map
                        (\( personId, person ) ->
                            fun
                                { personId = personId
                                , person = person
                                }
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
            { level = Level.empty { columns = 5, rows = 5 }
            , difficutly = 0
            , date = Date.first
            , nextEvents =
                [ ( Date.first
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
            , menu = Nothing
            , answeredMessages = Set.empty
            , messages = Dict.empty
            , filterMessages = Nothing
            , showMenu = False
            , trades =
                [ { add = Stone
                  , remove = [ ( Wood, 2 ) ]
                  }
                , { add = Wood
                  , remove = [ ( Stone, 2 ) ]
                  }
                ]
            , items = Data.ItemBag.empty
            , pointerZero = ( 0, 0 )
            , pointer = Nothing
            , persons =
                people
                    |> List.map
                        (\person ->
                            let
                                progress =
                                    0
                            in
                            { symbol = person.symbol
                            , name = person.name
                            , job = person.job
                            , progress = progress
                            , friendship = 0
                            , nextMessage =
                                Data.Person.next
                                    { job = person.job
                                    , progress = progress
                                    }
                                    |> Maybe.withDefault (Data.Person.defaultRequest Coin)
                            }
                        )
                    |> List.indexedMap Tuple.pair
                    |> Dict.fromList
            , stopGame = False
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
                    ([ []
                     , if event.reward && Data.ItemBag.size model.items < Data.ItemBag.maxAmountOfItems then
                        [ ShowNothing
                        , ShowItemAdded event.setting.reward
                        ]

                       else
                        []
                     , if event.mail then
                        [ ShowNothing
                        , ShowMail
                        ]

                       else
                        []
                     , if not (Date.summer model.date) && Date.day model.date == Date.daysInAMonth then
                        [ ShowNothing
                        , ShowEndscreen
                        , ShowEndscreen
                        , AdvanceYear
                        , ShowYear
                        ]

                       else
                        [ ShowNothing
                        , AdvanceCalenderDay
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

        AdvanceYear ->
            model |> nextDay

        ShowYear ->
            model

        ShowEndscreen ->
            { model | stopGame = True }

        ShowFriendship _ ->
            model

        ShowLover _ ->
            { model | stopGame = True }


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

        Reset ->
            model.history
                |> List.reverse
                |> List.head
                |> Maybe.map
                    (\level ->
                        ( { model
                            | level = level |> Level.setSelected Nothing
                            , history = []
                          }
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

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
                |> setMenuTab Nothing
            , NextActionBetweenDays
                |> Task.succeed
                |> Task.perform identity
            )

        SetSeed seed ->
            ( generateNextMonth model
                |> applyGenerator seed
            , Cmd.none
            )

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

        AcceptTrade args ->
            ( model
                |> tradeItems
                    { remove = args.remove
                    , add = [ args.add ]
                    }
            , Cmd.none
            )

        OpenMenu tab ->
            ( model
                |> showCalender
                |> setMenuTab (tab |> Just)
            , Cmd.none
            )

        CloseCalender ->
            ( { model | filterMessages = Nothing } |> closeCalender
            , Cmd.none
            )

        SetBetweenDays action ->
            ( model |> addBetweenDaysActions [ action ], Cmd.none )

        SetMenuTab menu ->
            ( model |> setMenuTab (Just menu), Cmd.none )

        AcceptMail i ->
            ( case Dict.get i model.messages of
                Just { message } ->
                    message.request
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
                            ([ message.present
                                |> Maybe.map
                                    (\item ->
                                        [ ShowNothing
                                        , ShowItemAdded item
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
            if model.stopGame then
                ( model, Cmd.none )

            else
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
                        , Process.sleep 700 |> Task.perform (\() -> NextActionBetweenDays)
                        )

                    [] ->
                        ( model, Cmd.none )

        ContinueGame ->
            { model | stopGame = False, betweenDays = List.drop 1 model.betweenDays }
                |> update NextActionBetweenDays

        SetFilterMessages person ->
            ( { model | filterMessages = person }, Cmd.none )


viewBetweenDays : BetweenDaysAction -> Model -> Html Msg
viewBetweenDays action model =
    case action of
        ShowCalenderDay ->
            Screen.BetweenDays.showCalender
                { date = model.date
                , events = model.nextEvents
                }

        AdvanceCalenderDay ->
            Screen.BetweenDays.showCalender
                { date = model.date
                , events = model.nextEvents
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

        AdvanceYear ->
            Screen.BetweenDays.showYear
                { year = Date.year model.date }

        ShowYear ->
            Screen.BetweenDays.showYear
                { year = Date.year model.date }

        ShowEndscreen ->
            Screen.BetweenDays.showEndscreen
                { onContinue = ContinueGame }

        ShowFriendship person ->
            Screen.BetweenDays.showFriendship person

        ShowLover person ->
            Screen.BetweenDays.showLover { onReject = ContinueGame } person


view : Model -> Html Msg
view model =
    [ Screen.Game.toHtml
        { items = model.items
        , level = model.level
        , unansweredMessages = Dict.size model.messages - Set.size model.answeredMessages
        , pointerZero = model.pointerZero
        , onPointerDown = PointerDown
        , onPointerEnd = PointerEnd
        , onPointerUp = PointerUp
        , onUndo = Undo
        , onReset = Reset
        , onSelectTab = OpenMenu
        }
    , (case model.menu of
        Just CalenderTab ->
            Screen.Menu.calender
                { date = model.date
                , events = model.nextEvents
                }

        Just MarketTab ->
            Screen.Menu.market
                { trades = model.trades
                , items = model.items
                , onAcceptTrade = AcceptTrade
                }

        Just MailTab ->
            Screen.Menu.messages
                { items = model.items
                , onAccept = AcceptMail
                , people = model.persons |> Dict.values
                , filter = model.filterMessages
                , onFilter = SetFilterMessages
                }
                (model.messages
                    |> Dict.toList
                    |> List.filterMap
                        (\( date, { personId, message } ) ->
                            model.persons
                                |> Dict.get personId
                                |> Maybe.map
                                    (\person ->
                                        { person = person
                                        , date = date
                                        , mail = message
                                        , answered = Set.member date model.answeredMessages
                                        }
                                    )
                        )
                )

        Nothing ->
            []
      )
        |> Screen.Menu.toHtml
            { show = model.showMenu
            , selected = model.menu
            , onClose = CloseCalender
            , onSelectTab = SetMenuTab
            , unansweredMessages = Dict.size model.messages - Set.size model.answeredMessages
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
