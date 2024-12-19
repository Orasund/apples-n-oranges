module Puzzle.Setting exposing (Event, Setting, pick, settings, specialSettings, startingLevel, toList)

import Data.Block exposing (Block(..), Item, Optional(..), Organic(..))
import Puzzle.Builder exposing (Group(..))
import Random


type alias Random a =
    Random.Generator a


type alias Event =
    { setting : Setting
    , reward : Maybe Item
    , mail : Bool
    }


type alias Setting =
    { symbol : Maybe Block
    , difficulty : Int
    , pairs : List ( Block, Block )
    }


empty : Setting
empty =
    { symbol = Nothing
    , difficulty = 0
    , pairs = []
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
        { symbol = OptionalBlock Rock |> Just
        , difficulty = args.difficulty
        , primary = ( Pickaxe, OptionalBlock Rock )
        , secondary = seasonalFruit { summer = args.summer }
        }


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


woodAndStone : { difficulty : Int, summer : Bool } -> Setting
woodAndStone args =
    template
        { symbol = Tree |> Just
        , difficulty = args.difficulty
        , primary = ( Axe, Tree )
        , secondary = ( Pickaxe, OptionalBlock Rock )
        }


summerDefault : { difficulty : Int, summer : Bool } -> Setting
summerDefault args =
    template
        { symbol = Just (OptionalBlock Fish)
        , difficulty = args.difficulty
        , primary = seasonalFruit { summer = args.summer }
        , secondary = ( FishingRod, OptionalBlock Fish )
        }


fishAndApples : { difficulty : Int, summer : Bool } -> Setting
fishAndApples args =
    template
        { symbol = Just (OptionalBlock Fish)
        , difficulty = args.difficulty
        , primary = ( FishingRod, OptionalBlock Fish )
        , secondary = seasonalFruit { summer = args.summer }
        }


winterDefault : { difficulty : Int, summer : Bool } -> Setting
winterDefault args =
    template
        { symbol = Just (OptionalBlock Rock)
        , difficulty = args.difficulty
        , primary = seasonalFruit { summer = args.summer }
        , secondary = ( Pickaxe, OptionalBlock Rock )
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
        , woodAndStone args |> withNoSymbol
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
