module Puzzle.Setting exposing (Event, Setting, pick, settings, specialSettings, startingLevel, toList)

import Data.Block exposing (Block(..), Flower(..), Item(..), Organic(..), Pastries(..))
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
    ( Block, Block )
    -> ( Block, Block )
    -> { difficulty : Int }
    -> Setting
template primary secondary { difficulty } =
    { empty
        | difficulty = difficulty
        , pairs =
            List.repeat (2 + max 0 (difficulty - 1)) primary
                ++ List.repeat (difficulty + 1) secondary
    }


settings : { difficulty : Int, summer : Bool } -> List Setting
settings args =
    let
        list =
            if args.summer then
                [ ( OrganicBlock Apple, OrganicBlock Orange )
                , ( Fish1, Fish2 )
                ]

            else
                [ ( OrganicBlock Potato, OrganicBlock Carrot )
                , ( Ice, Snow )
                ]
    in
    list
        |> List.concatMap
            (\pair1 ->
                list
                    |> List.map
                        (\pair2 ->
                            template pair1
                                pair2
                                { difficulty = args.difficulty }
                        )
            )


specialSettings : { difficulty : Int, summer : Bool } -> List Setting
specialSettings args =
    if args.summer then
        [ template
            ( FlowerBlock Hyacinth, FlowerBlock Sunflower )
            ( FlowerBlock Hyacinth, FlowerBlock Rose )
            { difficulty = args.difficulty + 1 }
            |> withReward Wood
        ]

    else
        [ template
            ( PastryBlock Prezel, PastryBlock Crouson )
            ( PastryBlock Prezel, PastryBlock Bagle )
            { difficulty = args.difficulty + 1 }
            |> withReward Stone
        ]


withReward : Item -> Setting -> Setting
withReward item setting =
    { setting
        | reward = item
    }


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
