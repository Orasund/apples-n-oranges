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
    , singles : List Block
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


rabbitAdvanced : Setting
rabbitAdvanced =
    let
        rabbitAndCarrotPairs =
            3
    in
    { empty
        | symbol = OptionalBlock Rabbit
        , difficulty = 2
        , pairs =
            [ ( OrganicBlock Apple, OrganicBlock Orange )
                |> List.repeat 6
            , ( OrganicBlock Carrot, OrganicBlock Apple ) |> List.repeat (rabbitAndCarrotPairs // 2)
            , ( OrganicBlock Carrot, OrganicBlock Orange ) |> List.repeat (rabbitAndCarrotPairs - rabbitAndCarrotPairs // 2)
            ]
                |> List.concat
        , singles =
            OptionalBlock Rabbit |> List.repeat (rabbitAndCarrotPairs * 2)
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
            , ( Rock, OptionalBlock Dynamite )
            , ( OrganicBlock Potato, OrganicBlock Carrot )
            ]
                |> List.repeat (n + 1)
                |> List.concat
        , singles = [ Rock ]
    }


rocksAndPotatos : Int -> Setting
rocksAndPotatos n =
    { empty
        | symbol = Rock
        , difficulty = n
        , pairs =
            [ ( Rock, OptionalBlock Dynamite )
            , ( OrganicBlock Potato, OrganicBlock Carrot )
            , ( Rock, OptionalBlock Dynamite )
            ]
                |> List.repeat (n + 1)
                |> List.concat
        , singles = [ OptionalBlock Dynamite ]
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
        , singles = [ OptionalBlock Fish ]
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
        , singles = [ OptionalBlock Fish ]
    }


tutorials : List Setting
tutorials =
    [ startingLevel
    , applesAndOranges 0
    , applesAndOranges 0
    , applesAndOranges 1
    , lemonsAndFish 0
    , rocksAndPotatos 0
    ]


settings : List Setting
settings =
    [ rabbitAdvanced
    , applesAndOranges 1
    , applesAndOranges 2
    , applesAndOranges 3
    , fishAndApples 1
    , fishAndApples 2
    , fishAndApples 3
    , potatosAndRocks 1
    , potatosAndRocks 2
    , potatosAndRocks 3
    , rocksAndPotatos 1
    , rocksAndPotatos 2
    , rocksAndPotatos 3
    , lemonsAndFish 1
    , lemonsAndFish 2
    , lemonsAndFish 3
    ]


pickSettings : { amount : Int, money : Int } -> Random (List Setting)
pickSettings args =
    let
        validSettings =
            settings
                |> List.filter
                    (\setting ->
                        priceForSetting setting <= args.money
                    )
                |> List.reverse
    in
    Random.list (List.length validSettings) (Random.float 0 1)
        |> Random.map
            (\randomFloat ->
                List.map2 Tuple.pair validSettings randomFloat
                    |> List.sortBy Tuple.second
                    |> List.take args.amount
                    |> List.map Tuple.first
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
        |> List.map SingleBlock
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
