module Level.Generator exposing (Setting, generate, pickSettings, priceForSetting)

import Dict
import Game exposing (Block(..), Game, Solid(..))
import Level exposing (Level)
import Level.Builder
import Random


type alias Random a =
    Random.Generator a


type alias Setting =
    { name : String
    , newFruitPairs : Int
    , newStone : Int
    , newDynamite : Int
    , newLemonPairs : Int
    , newGrapePairs : Int
    }


trainingGround1 : Setting
trainingGround1 =
    { name = "Training 1"
    , newFruitPairs = 2
    , newStone = 1
    , newDynamite = 0
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


trainingGround2 : Setting
trainingGround2 =
    { name = "Training 2"
    , newFruitPairs = 3
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


applesAndOrangesBasic : Setting
applesAndOrangesBasic =
    { name = "Apple'n Oranges 1"
    , newFruitPairs = 5
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


applesAndOranges : Setting
applesAndOranges =
    { name = "Apple'n Oranges 2"
    , newFruitPairs = 10
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


lifeGivesYouLemons : Setting
lifeGivesYouLemons =
    { name = "Life gives Lemons 1"
    , newFruitPairs = 0
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 10
    , newGrapePairs = 0
    }


lifeGivesYouLemonsBasic : Setting
lifeGivesYouLemonsBasic =
    { name = "Life gives Lemons 2"
    , newFruitPairs = 0
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 5
    , newGrapePairs = 0
    }


miningTime : Setting
miningTime =
    { name = "Mining Time 2"
    , newFruitPairs = 4
    , newStone = 8
    , newDynamite = 8
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


miningTimeBasic : Setting
miningTimeBasic =
    { name = "Mining Time 1"
    , newFruitPairs = 2
    , newStone = 4
    , newDynamite = 4
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


pleantyOfColor : Setting
pleantyOfColor =
    { name = "Pleanty of Color 2"
    , newFruitPairs = 2
    , newStone = 0
    , newDynamite = 0
    , newLemonPairs = 2
    , newGrapePairs = 5
    }


pleantyOfColorBasic : Setting
pleantyOfColorBasic =
    { name = "Pleanty of Color 1"
    , newFruitPairs = 0
    , newStone = 0
    , newDynamite = 0
    , newLemonPairs = 1
    , newGrapePairs = 3
    }


settings : List Setting
settings =
    [ trainingGround1
    , trainingGround2
    , applesAndOrangesBasic
    , applesAndOranges
    , lifeGivesYouLemonsBasic
    , lifeGivesYouLemons
    , miningTimeBasic
    , miningTime
    , pleantyOfColor
    , pleantyOfColorBasic
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
                |> List.take 3
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
        (setting.newFruitPairs
            + times 2 setting.newGrapePairs
            + times 1.5 setting.newLemonPairs
            + setting.newStone
            + setting.newDynamite
            - 3
        )


generate : Game -> Setting -> Random Level
generate game setting =
    let
        oldBlocks =
            game |> Game.getBlocks |> Dict.fromList

        oldStones =
            oldBlocks
                |> Dict.filter (\_ b -> b == SolidBlock Stone)
                |> Dict.size

        limitedByExistingStones n =
            min n (8 - oldStones)
    in
    Level.Builder.generateLevel
        { columns = 6
        , rows = 6
        , oldBlocks = oldBlocks
        , newSprouts = 0
        , newFruitPairs = setting.newFruitPairs --|> beginnerFriendly
        , newStone = setting.newStone |> limitedByExistingStones
        , newDynamite = setting.newDynamite --|> beginnerFriendly
        , newLemonPairs = setting.newLemonPairs --|> beginnerFriendly
        , newGrapePairs = setting.newGrapePairs --|> beginnerFriendly
        }
