module Data.Block exposing (..)


type Organic
    = Apple
    | Orange
    | Lemon
    | Carrot
    | Potato


type Optional
    = Dynamite
    | Fish
    | Rabbit


type Block
    = OrganicBlock Organic
    | OptionalBlock Optional
    | FishingRod
    | Rock


toString : Block -> String
toString block =
    case block of
        OrganicBlock Apple ->
            "🍎"

        OrganicBlock Orange ->
            "🍊"

        OrganicBlock Lemon ->
            "🍋"

        OrganicBlock Carrot ->
            "🥕"

        OrganicBlock Potato ->
            "🥔"

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
