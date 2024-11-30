module Block exposing (..)


type Fruit
    = Apple
    | Orange
    | Lemon
    | Grapes
    | Carrot


type Optional
    = Dynamite
    | Fish
    | Rabbit


type Block
    = FruitBlock Fruit
    | OptionalBlock Optional
    | FishingRod
    | Rock


toString : Block -> String
toString block =
    case block of
        FruitBlock Apple ->
            "🍎"

        FruitBlock Orange ->
            "🍊"

        FruitBlock Lemon ->
            "🍋"

        FruitBlock Grapes ->
            "🍇"

        FruitBlock Carrot ->
            "🥕"

        FishingRod ->
            "🎣"

        Rock ->
            "🪨"

        OptionalBlock Dynamite ->
            "💣"

        OptionalBlock Fish ->
            "🐟"

        OptionalBlock Rabbit ->
            "🐇"
