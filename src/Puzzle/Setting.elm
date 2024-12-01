module Puzzle.Setting exposing (Setting, pickSettings, priceForSetting, settings, shuffle, startingLevel, toBag, toGroups, tutorials)

import Bag exposing (Bag)
import Data.Block exposing (Block(..), Optional(..), Organic(..))
import Puzzle.Builder exposing (Group(..))
import Random


type alias Random a =
    Random.Generator a


type alias Setting =
    { symbol : Block
    , difficulty : Int
    , singles : List Optional
    , pairs : List ( Block, Block )
    }


empty : Setting
empty =
    { symbol = OrganicBlock Apple
    , difficulty = 0
    , singles = []
    , pairs = []
    }


startingLevel : Setting
startingLevel =
    { empty
        | symbol = OrganicBlock Orange
        , difficulty = 0
        , pairs =
            ( OrganicBlock Apple, OrganicBlock Orange )
                |> List.repeat 2
    }


applesAndOranges : Int -> Setting
applesAndOranges n =
    { empty
        | symbol = OrganicBlock Apple
        , difficulty = n
        , pairs =
            ( OrganicBlock Apple, OrganicBlock Orange )
                |> List.repeat (3 + n * 2)
    }


potatosAndRocks : Int -> Setting
potatosAndRocks n =
    { empty
        | symbol = OrganicBlock Potato
        , difficulty = n
        , pairs =
            [ ( OrganicBlock Potato, OrganicBlock Carrot )
            , ( Dynamite, OptionalBlock Rock )
            , ( OrganicBlock Potato, OrganicBlock Carrot )
            ]
                |> List.repeat (n + 1)
                |> List.concat
        , singles =
            Rabbit
                |> List.repeat ((n - 1) * 2)
    }


rocksAndPotatos : Int -> Setting
rocksAndPotatos n =
    { empty
        | symbol = OptionalBlock Rock
        , difficulty = n
        , pairs =
            [ ( Dynamite, OptionalBlock Rock )
            , ( OrganicBlock Potato, OrganicBlock Carrot )
            , ( Dynamite, OptionalBlock Rock )
            ]
                |> List.repeat (n + 1)
                |> List.concat
        , singles = [ Rock ]
    }


fishAndApples : Int -> Setting
fishAndApples n =
    { empty
        | symbol = OptionalBlock Fish
        , pairs =
            [ ( FishingRod, OptionalBlock Fish )
            , ( OrganicBlock Apple, OrganicBlock Orange )
            , ( FishingRod, OptionalBlock Fish )
            ]
                |> List.repeat (n + 1)
                |> List.concat
        , singles = [ Fish ]
    }


lemonsAndFish : Int -> Setting
lemonsAndFish n =
    { empty
        | symbol = OrganicBlock Lemon
        , difficulty = n
        , pairs =
            [ ( OrganicBlock Lemon, OrganicBlock Apple )
            , ( FishingRod, OptionalBlock Fish )
            , ( OrganicBlock Lemon, OrganicBlock Orange )
            ]
                |> List.repeat (n + 1)
                |> List.concat
        , singles = [ Fish ]
    }


tutorials : List Setting
tutorials =
    [ startingLevel
    , applesAndOranges 0
    , applesAndOranges 0
    , applesAndOranges 1
    , lemonsAndFish 0
    , fishAndApples 0
    ]


settings : { difficulty : Int, summer : Bool } -> List Setting
settings args =
    if args.summer then
        [ applesAndOranges args.difficulty
        , fishAndApples args.difficulty
        , lemonsAndFish args.difficulty
        ]

    else
        [ applesAndOranges args.difficulty
        , potatosAndRocks args.difficulty
        , rocksAndPotatos args.difficulty
        ]


pickSettings : { difficulty : Float, summer : Bool } -> Random Setting
pickSettings args =
    Random.float args.difficulty (args.difficulty + 1)
        |> Random.andThen
            (\float ->
                case
                    settings
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
