module Data.Block exposing (..)


type Organic
    = Apple
    | Orange
    | Lemon
    | Carrot
    | Potato


type Optional
    = Fish
    | TropicalFish
    | Rock
    | Coin
    | Diamand


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

        OptionalBlock TropicalFish ->
            "🐠"

        OptionalBlock Coin ->
            "🪙"

        OptionalBlock Diamand ->
            "💎"

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
            , ( Pickaxe, OptionalBlock Diamand )
            , ( FishingRod, OptionalBlock Fish )
            , ( FishingRod, OptionalBlock TropicalFish )
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
