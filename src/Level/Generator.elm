module Level.Generator exposing (Setting, generate, pickSetting, priceForSetting, trainingGround1, tutorials)

import Dict
import Game exposing (Block(..), Fruit(..), Game, Solid(..))
import Level exposing (Level)
import Level.Builder
import Random


type alias Random a =
    Random.Generator a


type alias Setting =
    { name : String
    , symbol : Block
    , newFruitPairs : Int
    , newStone : Int
    , newDynamite : Int
    , newLemonPairs : Int
    , newGrapePairs : Int
    }


trainingGround1 : Setting
trainingGround1 =
    { name = "Training 1"
    , symbol = FruitBlock Apple
    , newFruitPairs = 2
    , newStone = 1
    , newDynamite = 0
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


trainingGround2 : Setting
trainingGround2 =
    { name = "Training 2"
    , symbol = SolidBlock Stone
    , newFruitPairs = 3
    , newStone = 1
    , newDynamite = 0
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


trainingGround3 : Setting
trainingGround3 =
    { name = "Training 3"
    , symbol = FruitBlock Lemon
    , newFruitPairs = 3
    , newStone = 1
    , newDynamite = 0
    , newLemonPairs = 1
    , newGrapePairs = 0
    }


trainingGround4 : Setting
trainingGround4 =
    { name = "Training 3"
    , symbol = SolidBlock Dynamite
    , newFruitPairs = 3
    , newStone = 1
    , newDynamite = 2
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


applesAndOrangesBasic : Setting
applesAndOrangesBasic =
    { name = "Apple'n Oranges 1"
    , symbol = FruitBlock Orange
    , newFruitPairs = 5
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


applesAndOranges : Setting
applesAndOranges =
    { name = "Apple'n Oranges 2"
    , symbol = FruitBlock Orange
    , newFruitPairs = 10
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


lifeGivesYouLemons : Setting
lifeGivesYouLemons =
    { name = "Life gives Lemons 1"
    , symbol = FruitBlock Lemon
    , newFruitPairs = 5
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 5
    , newGrapePairs = 0
    }


lifeGivesYouLemonsBasic : Setting
lifeGivesYouLemonsBasic =
    { name = "Life gives Lemons 2"
    , symbol = FruitBlock Lemon
    , newFruitPairs = 2
    , newStone = 1
    , newDynamite = 1
    , newLemonPairs = 3
    , newGrapePairs = 0
    }


miningTimeBasic : Setting
miningTimeBasic =
    { name = "Mining Time 1"
    , symbol = SolidBlock Dynamite
    , newFruitPairs = 4
    , newStone = 3
    , newDynamite = 3
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


miningTime : Setting
miningTime =
    { name = "Mining Time 2"
    , symbol = SolidBlock Dynamite
    , newFruitPairs = 6
    , newStone = 6
    , newDynamite = 6
    , newLemonPairs = 0
    , newGrapePairs = 0
    }


pleantyOfColorBasic : Setting
pleantyOfColorBasic =
    { name = "Pleanty of Color 1"
    , symbol = FruitBlock Grapes
    , newFruitPairs = 3
    , newStone = 0
    , newDynamite = 0
    , newLemonPairs = 0
    , newGrapePairs = 2
    }


pleantyOfColor : Setting
pleantyOfColor =
    { name = "Pleanty of Color 2"
    , symbol = FruitBlock Grapes
    , newFruitPairs = 6
    , newStone = 0
    , newDynamite = 0
    , newLemonPairs = 0
    , newGrapePairs = 3
    }


tutorials : List Setting
tutorials =
    [ trainingGround1
    , trainingGround2
    , trainingGround3
    , trainingGround4
    ]


settings : List Setting
settings =
    [ applesAndOrangesBasic
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


pickSetting : { money : Int } -> Random Setting
pickSetting args =
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
    case validSettings of
        [] ->
            Random.constant applesAndOranges

        head :: tail ->
            Random.uniform head tail


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
            - 10
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
