module Puzzle.Setting exposing (Setting, pick, priceForSetting, settings, shuffle, specialSettings, startingLevel, toBag, toGroups, tutorials)

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
            [ ( Dynamite, OptionalBlock Rock )
            , ( OrganicBlock f1, OrganicBlock f2 )
            , ( Dynamite, OptionalBlock Rock )
            ]
                |> List.repeat (args.difficulty + 1)
                |> List.concat
        , singles = [ Rock ]
    }


fishAndApples : Int -> Setting
fishAndApples n =
    { empty
        | symbol = OptionalBlock Fish |> Just
        , pairs =
            [ ( FishingRod, OptionalBlock Fish )
            , ( OrganicBlock Apple, OrganicBlock Orange )
            , ( FishingRod, OptionalBlock Fish )
            ]
                |> List.repeat (n + 1)
                |> List.concat
        , singles = [ Fish ]
    }


lemonsAndFish : { difficulty : Int, summer : Bool } -> Setting
lemonsAndFish args =
    let
        ( f1, f2 ) =
            seasonalFruit { summer = args.summer }
    in
    { empty
        | symbol = OrganicBlock Lemon |> Just
        , difficulty = args.difficulty
        , pairs =
            [ ( OrganicBlock Lemon, OrganicBlock f1 )
            , ( FishingRod, OptionalBlock Fish )
            , ( OrganicBlock Lemon, OrganicBlock f2 )
            ]
                |> List.repeat (args.difficulty + 1)
                |> List.concat
        , singles = [ Fish ]
    }


seasonal : { difficulty : Int, summer : Bool } -> Setting
seasonal args =
    let
        ( f1, f2 ) =
            seasonalFruit { summer = args.summer }

        ( s1, s2 ) =
            if args.summer then
                ( FishingRod, Fish )

            else
                ( Dynamite, Rock )
    in
    { empty
        | symbol = OrganicBlock f2 |> Just
        , difficulty = args.difficulty
        , pairs =
            [ ( OrganicBlock f1, OrganicBlock f2 )
            , ( s1, OptionalBlock s2 )
            , ( OrganicBlock f1, OrganicBlock f2 )
            ]
                |> List.repeat (args.difficulty + 1)
                |> List.concat
        , singles = [ s2 ]
    }


tutorials : List Setting
tutorials =
    let
        args =
            { difficulty = 1, summer = True }
    in
    [ default { args | difficulty = 0 }
    , default { args | difficulty = 0 }
    , default args
    , seasonal { args | difficulty = 0 }
    , default args
    , seasonal { args | difficulty = 0 }
    , fishAndApples 0
    ]


settings : { difficulty : Int, summer : Bool } -> List Setting
settings args =
    [ default args
    , seasonal args
    ]


specialSettings : { difficulty : Int, summer : Bool } -> List Setting
specialSettings args =
    if args.summer then
        [ lemonsAndFish args
        , fishAndApples args.difficulty
        ]

    else
        [ rocks args
        ]


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
