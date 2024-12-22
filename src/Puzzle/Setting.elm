module Puzzle.Setting exposing (Event, Setting, pick, settings, specialSettings, startingLevel, toList)

import Data.Block exposing (Block(..), Flower(..), Item(..), Organic(..))
import Puzzle.Builder exposing (Group(..))
import Random


type alias Random a =
    Random.Generator a


type alias Event =
    { setting : Setting
    , reward : Bool
    , mail : Bool
    }


type alias Setting =
    { symbol : Maybe Block
    , difficulty : Int
    , pairs : List ( Block, Block )
    , reward : Item
    }


empty : Setting
empty =
    { symbol = Nothing
    , difficulty = 0
    , pairs = []
    , reward = Coin
    }


seasonalFruit : { summer : Bool } -> ( Block, Block )
seasonalFruit args =
    if args.summer then
        ( OrganicBlock Apple, OrganicBlock Orange )

    else
        ( OrganicBlock Potato, OrganicBlock Carrot )


startingLevel : Setting
startingLevel =
    { empty
        | symbol = Nothing
        , difficulty = 0
        , pairs =
            ( OrganicBlock Apple, OrganicBlock Orange )
                |> List.repeat 2
    }


{-| level 0 (3): 2+0+1
level 1 (4): 2+0+2
level 2 (6): 2+1+3
level 3 (8): 2+2+4
-}
template :
    { difficulty : Int
    , symbol : Maybe Block
    , primary : ( Block, Block )
    , secondary : ( Block, Block )
    }
    -> Setting
template args =
    { empty
        | difficulty = args.difficulty
        , symbol = args.symbol
        , pairs =
            List.repeat (2 + max 0 (args.difficulty - 1)) args.primary
                ++ List.repeat (args.difficulty + 1) args.secondary
    }


default : { difficulty : Int, summer : Bool } -> Setting
default args =
    template
        { symbol = Nothing
        , difficulty = args.difficulty
        , primary = seasonalFruit { summer = args.summer }
        , secondary = seasonalFruit { summer = args.summer }
        }


rocks : { difficulty : Int, summer : Bool } -> Setting
rocks args =
    template
        { symbol = Rock |> Just
        , difficulty = args.difficulty
        , primary = ( Pickaxe, Rock )
        , secondary = seasonalFruit { summer = args.summer }
        }
        |> withReward Diamand


lemons : { difficulty : Int, summer : Bool } -> Setting
lemons args =
    let
        ( f1, _ ) =
            seasonalFruit { summer = args.summer }
    in
    template
        { symbol = OrganicBlock Lemon |> Just
        , difficulty = args.difficulty
        , primary = ( OrganicBlock Lemon, f1 )
        , secondary = seasonalFruit { summer = args.summer }
        }
        |> withReward Berries


flowers : { difficulty : Int, summer : Bool } -> Setting
flowers args =
    template
        { symbol = FlowerBlock Sunflower |> Just
        , difficulty = args.difficulty
        , primary = ( FlowerBlock Hyacinth, FlowerBlock Sunflower )
        , secondary = ( FlowerBlock Rose, FlowerBlock Sunflower )
        }
        |> withReward Berries


woodAndStone : { difficulty : Int, summer : Bool } -> Setting
woodAndStone args =
    template
        { symbol = Tree2 |> Just
        , difficulty = args.difficulty
        , primary = ( Tree1, Tree2 )
        , secondary = ( Pickaxe, Rock )
        }
        |> withReward Wood


mushroomAndWoods : { difficulty : Int, summer : Bool } -> Setting
mushroomAndWoods args =
    template
        { symbol = Mushroom1 |> Just
        , difficulty = args.difficulty
        , primary = ( Mushroom1, Mushroom2 )
        , secondary = ( Tree1, Tree2 )
        }
        |> withReward Wood


summerDefault : { difficulty : Int, summer : Bool } -> Setting
summerDefault args =
    template
        { symbol = Just Fish1
        , difficulty = args.difficulty
        , primary = seasonalFruit { summer = args.summer }
        , secondary = ( Fish2, Fish1 )
        }


fishAndApples : { difficulty : Int, summer : Bool } -> Setting
fishAndApples args =
    template
        { symbol = Just Fish1
        , difficulty = args.difficulty
        , primary = ( Fish2, Fish1 )
        , secondary = seasonalFruit { summer = args.summer }
        }
        |> withReward Shrimps


winterDefault : { difficulty : Int, summer : Bool } -> Setting
winterDefault args =
    template
        { symbol = Just Rock
        , difficulty = args.difficulty
        , primary = seasonalFruit { summer = args.summer }
        , secondary = ( Pickaxe, Rock )
        }


settings : { difficulty : Int, summer : Bool } -> List Setting
settings args =
    if args.summer then
        [ default args
        , default args
        , summerDefault args |> withNoSymbol
        , summerDefault args |> withNoSymbol
        , flowers args |> withNoSymbol
        ]

    else
        [ default args
        , default args
        , winterDefault args |> withNoSymbol
        , winterDefault args |> withNoSymbol

        -- , mushroomAndWoods args |> withNoSymbol
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
        [ woodAndStone { args | difficulty = args.difficulty + 1 }
        , woodAndStone { args | difficulty = args.difficulty + 1 }
        , rocks { args | difficulty = args.difficulty + 1 }
        , rocks { args | difficulty = args.difficulty + 1 }
        , summerDefault { args | difficulty = args.difficulty + 1 }
        ]


withNoSymbol : Setting -> Setting
withNoSymbol setting =
    { setting | symbol = Nothing }


withReward : Item -> Setting -> Setting
withReward item setting =
    { setting | reward = item }


pick : { difficulty : Float, summer : Bool } -> ({ difficulty : Int, summer : Bool } -> List Setting) -> Random Setting
pick args fun =
    Random.float 0 (min 4 (args.difficulty + 1))
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


toList : Setting -> List ( Block, Block )
toList setting =
    setting.pairs
