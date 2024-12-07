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
    | Coin


type Block
    = OrganicBlock Organic
    | OptionalBlock Optional
    | FishingRod
    | Pickaxe
    | Axe
    | Wood


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

        OptionalBlock Rock ->
            "🪨"

        OptionalBlock Fish ->
            "🐟"

        OptionalBlock Coin ->
            "🪙"

        FishingRod ->
            "🎣"

        Pickaxe ->
            "⛏️"

        Axe ->
            "🪓"

        Wood ->
            "🪵"


isValidBlock : Block -> Block -> Bool
isValidBlock p1 p2 =
    case ( p1, p2 ) of
        ( OrganicBlock _, OrganicBlock _ ) ->
            True

        _ ->
            [ ( Pickaxe, OptionalBlock Rock )
            , ( FishingRod, OptionalBlock Fish )
            , ( OptionalBlock Coin, OptionalBlock Fish )
            , ( Axe, Wood )
            ]
                |> List.any
                    (\pair ->
                        ( p1, p2 ) == pair || ( p2, p1 ) == pair
                    )


isOptional : Block -> Bool
isOptional block =
    case block of
        OptionalBlock _ ->
            True

        _ ->
            False


isPersistant : Block -> Bool
isPersistant _ =
    False
