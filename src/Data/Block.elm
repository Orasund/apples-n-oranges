module Data.Block exposing (..)


type Organic
    = Apple
    | Orange
    | Lemon
    | Carrot
    | Potato


type Optional
    = Fish
    | Rock


type Block
    = OrganicBlock Organic
    | OptionalBlock Optional
    | FishingRod
    | Dynamite


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

        Dynamite ->
            "⛏️"

        OptionalBlock Rock ->
            "🪨"

        OptionalBlock Fish ->
            "🐟"
