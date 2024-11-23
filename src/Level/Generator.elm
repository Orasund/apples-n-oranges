module Level.Generator exposing (Setting, generate, pickSettings, priceForSetting, settings, sort, startingLevel, tutorials)

import Dict
import Game exposing (Block(..), Fruit(..), Game, Solid(..))
import Level exposing (Level)
import Level.Builder
import Random


type alias Random a =
    Random.Generator a


type alias Setting =
    { symbol : Block
    , difficulty : Int
    , newFruitPairs : Int
    , newStone : Int
    , newDynamite : Int
    , newLemonPairs : Int
    , newGrapePairs : Int
    }


startingLevel : Setting
startingLevel =
    { symbol = FruitBlock Apple
    , difficulty = 0
    , newFruitPairs = 2
    , newStone = 0
    , newDynamite = 0
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


applesTraining : Setting
applesTraining =
    { symbol = FruitBlock Apple
    , difficulty = 0
    , newFruitPairs = 3
    , newStone = 0
    , newDynamite = 0
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


applesBasic : Setting
applesBasic =
    { symbol = FruitBlock Apple
    , difficulty = 1
    , newFruitPairs = 5
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


applesAdvanced : Setting
applesAdvanced =
    { symbol = FruitBlock Apple
    , difficulty = 2
    , newFruitPairs = 10
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


lemonTraining : Setting
lemonTraining =
    { symbol = FruitBlock Lemon
    , difficulty = 0
    , newFruitPairs = 3
    , newStone = 1
    , newDynamite = 0
    , newLemonPairs = 1
    , newGrapePairs = 0
    }


lemonBasic : Setting
lemonBasic =
    { symbol = FruitBlock Lemon
    , difficulty = 1
    , newFruitPairs = 2
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 3
    , newGrapePairs = 0
    }


lemonAdvanced : Setting
lemonAdvanced =
    { symbol = FruitBlock Lemon
    , difficulty = 2
    , newFruitPairs = 5
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 5
    , newGrapePairs = 0
    }


miningTraining : Setting
miningTraining =
    { symbol = SolidBlock Dynamite
    , difficulty = 0
    , newFruitPairs = 3
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


miningBasic : Setting
miningBasic =
    { symbol = SolidBlock Dynamite
    , difficulty = 1
    , newFruitPairs = 4
    , newStone = 3
    , newDynamite = 3
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


miningAdvanced : Setting
miningAdvanced =
    { symbol = SolidBlock Dynamite
    , difficulty = 2
    , newFruitPairs = 6
    , newStone = 6
    , newDynamite = 6
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


grapesBasic : Setting
grapesBasic =
    { symbol = FruitBlock Grapes
    , difficulty = 1
    , newFruitPairs = 3
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 0
    , newGrapePairs = 2
    }


grapesAdvanced : Setting
grapesAdvanced =
    { symbol = FruitBlock Grapes
    , difficulty = 2
    , newFruitPairs = 6
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 0
    , newGrapePairs = 3
    }


tutorials : List Setting
tutorials =
    [ applesTraining
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
            round (toFloat n * float)
    in
    max 0
        ((times 0.7 setting.newFruitPairs
            + times 1.5 setting.newGrapePairs
            + times 1 setting.newLemonPairs
            + times 0.5 setting.newStone
            + times 0.5 setting.newDynamite
          -- 3
         )
            ^ 2
        )


generate : Game -> Setting -> Random Level
generate game setting =
    let
        oldBlocks =
            game |> Game.getBlocks |> Dict.fromList

        oldStones block =
            oldBlocks
                |> Dict.filter (\_ b -> b == block)
                |> Dict.size

        limitedByExisting block n =
            min n (8 - oldStones block)
    in
    Level.Builder.generateLevel
        { columns = 6
        , rows = 6
        , oldBlocks = oldBlocks
        , newSprouts = 0
        , newFruitPairs = setting.newFruitPairs
        , newStone = setting.newStone |> limitedByExisting (SolidBlock Stone)
        , newDynamite = setting.newDynamite |> limitedByExisting (SolidBlock Dynamite)
        , newLemonPairs = setting.newLemonPairs
        , newGrapePairs = setting.newGrapePairs
        }


sort : List a -> Random (List a)
sort list =
    Random.list (List.length list) (Random.float 0 1)
        |> Random.map
            (\randomList ->
                List.map2 Tuple.pair
                    list
                    randomList
                    |> List.sortBy Tuple.second
                    |> List.map Tuple.first
            )
