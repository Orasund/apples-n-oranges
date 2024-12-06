module Puzzle.Setting exposing (Setting, pick, priceForSetting, settings, shuffle, specialSettings, startingLevel, toBag, toGroups)

import Bag exposing (Bag)
import Data.Block exposing (Block(..), Optional(..), Organic(..))
import Puzzle.Builder exposing (Group(..))
import Random


type alias Random a =
    Random.Generator a


type alias Setting =
    { symbol : Maybe Block
    , difficulty : Int
    , singles : List Optional
    , pairs : List ( Block, Block )
    }


empty : Setting
empty =
    { symbol = Nothing
    , difficulty = 0
    , singles = []
    , pairs = []
    }


seasonalFruit : { summer : Bool } -> ( Organic, Organic )
seasonalFruit args =
    if args.summer then
        ( Apple, Orange )

    else
        ( Potato, Carrot )


startingLevel : Setting
startingLevel =
    { empty
        | symbol = Nothing
        , difficulty = 0
        , pairs =
            ( OrganicBlock Apple, OrganicBlock Orange )
                |> List.repeat 2
    }


default : { difficulty : Int, summer : Bool } -> Setting
default args =
    let
        ( f1, f2 ) =
            seasonalFruit { summer = args.summer }
    in
    { empty
        | symbol = Nothing
        , difficulty = args.difficulty
        , pairs =
            ( OrganicBlock f1, OrganicBlock f2 )
                |> List.repeat (3 + args.difficulty * 3 // 2)
    }


rocks : { difficulty : Int, summer : Bool } -> Setting
rocks args =
    let
        ( f1, f2 ) =
            seasonalFruit { summer = args.summer }
    in
    { empty
        | symbol = OptionalBlock Rock |> Just
        , difficulty = args.difficulty
        , pairs =
            [ ( Pickaxe, OptionalBlock Rock )
            , ( OrganicBlock f1, OrganicBlock f2 )
            , ( Pickaxe, OptionalBlock Rock )
            ]
                |> List.repeat (args.difficulty + 1)
                |> List.concat
        , singles = Rock |> List.repeat args.difficulty
    }


lemons : { difficulty : Int, summer : Bool } -> Setting
lemons args =
    let
        ( f1, f2 ) =
            seasonalFruit { summer = args.summer }
    in
    { empty
        | symbol = OrganicBlock Lemon |> Just
        , difficulty = args.difficulty
        , pairs =
            [ ( OrganicBlock Lemon, OrganicBlock f1 )
            , ( OrganicBlock f1, OrganicBlock f2 )
            , ( OrganicBlock Lemon, OrganicBlock f2 )
            ]
                |> List.repeat (args.difficulty + 1)
                |> List.concat
        , singles = []
    }


fireAndStone : { difficulty : Int, summer : Bool } -> Setting
fireAndStone args =
    { empty
        | symbol = Fire |> Just
        , difficulty = args.difficulty
        , pairs =
            [ ( Fire, Wood )
            ]
                |> List.repeat ((args.difficulty + 1) * 2)
                |> List.concat
        , singles = Rock |> List.repeat (args.difficulty + 1)
    }


summerDefault : { difficulty : Int, summer : Bool } -> Setting
summerDefault args =
    let
        ( f1, f2 ) =
            seasonalFruit { summer = args.summer }

        ( s1, s2 ) =
            ( FishingRod, Fish )
    in
    { empty
        | symbol = Just (OptionalBlock s2)
        , difficulty = args.difficulty
        , pairs =
            [ ( OrganicBlock f1, OrganicBlock f2 )
            , ( s1, OptionalBlock s2 )
            , ( OrganicBlock f1, OrganicBlock f2 )
            ]
                |> List.repeat (args.difficulty + 1)
                |> List.concat
        , singles = s2 |> List.repeat args.difficulty
    }


fishAndApples : { difficulty : Int, summer : Bool } -> Setting
fishAndApples args =
    let
        ( f1, f2 ) =
            seasonalFruit { summer = args.summer }
    in
    { empty
        | symbol = OptionalBlock Fish |> Just
        , difficulty = args.difficulty
        , pairs =
            [ ( FishingRod, OptionalBlock Fish )
            , ( OrganicBlock f1, OrganicBlock f2 )
            , ( FishingRod, OptionalBlock Fish )
            ]
                |> List.repeat (args.difficulty + 1)
                |> List.concat
        , singles = Fish |> List.repeat args.difficulty
    }


winterDefault : { difficulty : Int, summer : Bool } -> Setting
winterDefault args =
    let
        ( f1, f2 ) =
            seasonalFruit { summer = args.summer }

        ( s1, s2 ) =
            ( Pickaxe, Rock )
    in
    { empty
        | symbol = Just (OptionalBlock s2)
        , difficulty = args.difficulty
        , pairs =
            [ ( OrganicBlock f1, OrganicBlock f2 )
            , ( s1, OptionalBlock s2 )
            , ( OrganicBlock f1, OrganicBlock f2 )
            ]
                |> List.repeat (args.difficulty + 1)
                |> List.concat
        , singles = s2 |> List.repeat args.difficulty
    }


settings : { difficulty : Int, summer : Bool } -> List Setting
settings args =
    if args.summer then
        [ default args
        , default args
        , summerDefault args |> withNoSymbol
        , summerDefault args |> withNoSymbol
        , lemons args |> withNoSymbol
        ]

    else
        [ default args
        , default args
        , winterDefault args |> withNoSymbol
        , winterDefault args |> withNoSymbol
        , fireAndStone args |> withNoSymbol
        ]


specialSettings : { difficulty : Int, summer : Bool } -> List Setting
specialSettings args =
    if args.summer then
        [ lemons { args | difficulty = args.difficulty + 1 }
        , lemons { args | difficulty = args.difficulty + 1 }
        , fishAndApples { args | difficulty = args.difficulty + 1 }
        , fishAndApples { args | difficulty = args.difficulty + 1 }
        , winterDefault { args | difficulty = args.difficulty + 1 }
        ]

    else
        [ fireAndStone { args | difficulty = args.difficulty + 1 }
        , fireAndStone { args | difficulty = args.difficulty + 1 }
        , rocks { args | difficulty = args.difficulty + 1 }
        , rocks { args | difficulty = args.difficulty + 1 }
        , summerDefault { args | difficulty = args.difficulty + 1 }
        ]


withNoSymbol : Setting -> Setting
withNoSymbol setting =
    { setting | symbol = Nothing }


pick : { difficulty : Float, summer : Bool } -> ({ difficulty : Int, summer : Bool } -> List Setting) -> Random Setting
pick args fun =
    Random.float args.difficulty (args.difficulty + 1)
        |> Random.andThen
            (\float ->
                case
                    fun
                        { difficulty = floor float
                        , summer = args.summer
                        }
                of
                    head :: tail ->
                        Random.uniform head tail

                    [] ->
                        Random.constant empty
            )


priceForSetting : Setting -> Int
priceForSetting setting =
    max 0
        ((toFloat (List.length setting.pairs)
            + toFloat (List.length setting.singles)
         )
            ^ 1.6
        )
        |> round


toGroups : Setting -> List Group
toGroups setting =
    [ setting.singles
        |> List.map (\optional -> SingleBlock (OptionalBlock optional))
    , setting.pairs
        |> List.map (\( a, b ) -> Pair a b)
    ]
        |> List.concat


toBag : Setting -> Bag
toBag setting =
    Bag.empty
        |> Bag.insertAll
            (setting
                |> toGroups
                |> List.concatMap
                    (\group ->
                        case group of
                            Pair a b ->
                                [ a, b ]

                            SingleBlock a ->
                                [ a ]
                    )
            )


shuffle : List a -> Random (List a)
shuffle list =
    Random.list (List.length list) (Random.float 0 1)
        |> Random.map
            (\randomList ->
                List.map2 Tuple.pair
                    list
                    randomList
                    |> List.sortBy Tuple.second
                    |> List.map Tuple.first
            )
