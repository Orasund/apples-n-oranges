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


type Item
    = Coin
    | BagOfCoins
    | Diamand


type Block
    = OrganicBlock Organic
    | OptionalBlock Optional
    | ItemBlock Item
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

        ItemBlock Coin ->
            "🪙"

        ItemBlock BagOfCoins ->
            "💰"

        ItemBlock Diamand ->
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

        ( ItemBlock _, ItemBlock _ ) ->
            True

        _ ->
            [ ( Pickaxe, OptionalBlock Rock )
            , ( FishingRod, OptionalBlock Fish )
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

        ItemBlock _ ->
            True

        _ ->
            False


isPersistant : Block -> Bool
isPersistant _ =
    False
