module Puzzle.Setting exposing (Setting, generate, pickSettings, priceForSetting, settings, shuffle, startingLevel, tutorials)

import Dict
import Level exposing (Block(..), Fruit(..), Level, Optional(..), Puzzle, Solid(..))
import Puzzle.Builder
import Random


type alias Random a =
    Random.Generator a


type alias Setting =
    { symbol : Block
    , difficulty : Int
    , newFruitPairs : Int
    , newStoneAndDynamite : Int
    , newLemonPairs : Int
    , newGrapePairs : Int
    , rabbitAndCarrotPairs : Int
    , fishAndRod : Int
    }


empty : Setting
empty =
    { symbol = FruitBlock Apple
    , difficulty = 0
    , newFruitPairs = 0
    , newStoneAndDynamite = 0
    , newLemonPairs = 0
    , newGrapePairs = 0
    , rabbitAndCarrotPairs = 0
    , fishAndRod = 0
    }


startingLevel : Setting
startingLevel =
    { empty
        | symbol = FruitBlock Orange
        , difficulty = 0
        , newFruitPairs = 2
    }


applesTraining : Setting
applesTraining =
    { empty
        | symbol = FruitBlock Apple
        , difficulty = 0
        , newFruitPairs = 3
    }


applesBasic : Setting
applesBasic =
    { empty
        | symbol = FruitBlock Orange
        , difficulty = 1
        , newFruitPairs = 5
        , newStoneAndDynamite = 1
    }


applesAdvanced : Setting
applesAdvanced =
    { empty
        | symbol = FruitBlock Orange
        , difficulty = 2
        , newFruitPairs = 10
        , newStoneAndDynamite = 1
    }


lemonTraining : Setting
lemonTraining =
    { empty
        | symbol = FruitBlock Lemon
        , difficulty = 0
        , newFruitPairs = 3
        , newLemonPairs = 1
    }


lemonBasic : Setting
lemonBasic =
    { empty
        | symbol = FruitBlock Lemon
        , difficulty = 1
        , newFruitPairs = 2
        , newStoneAndDynamite = 1
        , newLemonPairs = 3
    }


lemonAdvanced : Setting
lemonAdvanced =
    { empty
        | symbol = FruitBlock Lemon
        , difficulty = 2
        , newFruitPairs = 5
        , newStoneAndDynamite = 1
        , newLemonPairs = 5
    }


miningTraining : Setting
miningTraining =
    { empty
        | symbol = OptionalBlock Dynamite
        , difficulty = 0
        , newFruitPairs = 3
        , newStoneAndDynamite = 1
    }


miningBasic : Setting
miningBasic =
    { empty
        | symbol = OptionalBlock Dynamite
        , difficulty = 1
        , newFruitPairs = 4
        , newStoneAndDynamite = 3
    }


miningAdvanced : Setting
miningAdvanced =
    { empty
        | symbol = OptionalBlock Dynamite
        , difficulty = 2
        , newFruitPairs = 6
        , newStoneAndDynamite = 6
    }


grapesBasic : Setting
grapesBasic =
    { empty
        | symbol = FruitBlock Grapes
        , difficulty = 1
        , newFruitPairs = 3
        , newStoneAndDynamite = 1
        , newGrapePairs = 2
    }


grapesAdvanced : Setting
grapesAdvanced =
    { empty
        | symbol = FruitBlock Grapes
        , difficulty = 2
        , newFruitPairs = 6
        , newStoneAndDynamite = 1
        , newGrapePairs = 3
    }


fishingBasic : Setting
fishingBasic =
    { empty
        | symbol = FishingRod
        , difficulty = 1
        , newFruitPairs = 5
        , fishAndRod = 4
    }


fishingAdvanced : Setting
fishingAdvanced =
    { empty
        | symbol = FishingRod
        , difficulty = 2
        , newFruitPairs = 5
        , fishAndRod = 6
    }


rabbitAdvanced : Setting
rabbitAdvanced =
    { empty
        | symbol = OptionalBlock Rabbit
        , difficulty = 2
        , rabbitAndCarrotPairs = 3
        , newFruitPairs = 6
    }


tutorials : List Setting
tutorials =
    [ startingLevel
    , applesTraining
    , applesTraining
    , applesTraining
    , lemonTraining
    , miningTraining
    ]


settings : List Setting
settings =
    [ applesTraining
    , applesBasic
    , applesAdvanced
    , lemonTraining
    , lemonBasic
    , lemonAdvanced
    , miningTraining
    , miningBasic
    , miningAdvanced
    , grapesBasic
    , grapesAdvanced
    , fishingBasic
    , fishingAdvanced
    , rabbitAdvanced
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

        --|> List.take 5
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
    let
        times float n =
            toFloat n * float
    in
    max 0
        ((times 1 setting.newFruitPairs
            + times 1 setting.newGrapePairs
            + times 1 setting.newLemonPairs
            + times 1 setting.newStoneAndDynamite
            + times 1 setting.fishAndRod
            + times 2 setting.rabbitAndCarrotPairs
         )
            ^ 1.6
         -- - 6
        )
        |> round


generate : Level -> Setting -> Random Puzzle
generate game setting =
    let
        oldBlocks =
            game |> Level.getBlocks |> Dict.fromList

        oldStones block =
            oldBlocks
                |> Dict.filter (\_ b -> b == block)
                |> Dict.size

        limitedByExisting block n =
            min n (8 - oldStones block)
    in
    Puzzle.Builder.generateLevel
        { columns = 6
        , rows = 6
        , oldBlocks = oldBlocks

        --, newSprouts = 0
        , newFruitPairs = setting.newFruitPairs
        , newStone = setting.newStoneAndDynamite |> limitedByExisting (SolidBlock Stone)
        , newDynamite = setting.newStoneAndDynamite
        , newLemonPairs = setting.newLemonPairs
        , newGrapePairs = setting.newGrapePairs
        , rabbitAndCarrotPairs = setting.rabbitAndCarrotPairs
        , fishAndRod = setting.fishAndRod
        }


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
