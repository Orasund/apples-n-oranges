module Data.Block exposing (..)


type Organic
    = Apple
    | Orange
    | Lemon
    | Carrot
    | Potato


type Flower
    = Sunflower
    | Rose
    | Hyacinth


type Item
    = Coin
    | BagOfCoins
    | Diamand
    | Chick
    | Wood
    | Shrimps
    | Berries


type Block
    = OrganicBlock Organic
    | FlowerBlock Flower
    | ItemBlock Item
    | Rock
    | Pickaxe
    | Fish1
    | Fish2
    | Tree1
    | Tree2
    | Mushroom1
    | Mushroom2


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

        ItemBlock Coin ->
            "🪙"

        ItemBlock BagOfCoins ->
            "💰"

        ItemBlock Diamand ->
            "💎"

        ItemBlock Chick ->
            "🐥"

        ItemBlock Wood ->
            "🪵"

        ItemBlock Shrimps ->
            "🦐"

        ItemBlock Berries ->
            "🍇"

        Rock ->
            "🪨"

        Pickaxe ->
            "⛏️"

        Fish1 ->
            "🐟"

        Fish2 ->
            "🎣"

        Tree1 ->
            "🌲"

        Tree2 ->
            "🌳"

        FlowerBlock Sunflower ->
            "🌻"

        FlowerBlock Rose ->
            "🌹"

        FlowerBlock Hyacinth ->
            "\u{1FABB}"

        Mushroom1 ->
            "🍄"

        Mushroom2 ->
            "🍄\u{200D}🟫"


isValidBlock : Block -> Block -> Bool
isValidBlock p1 p2 =
    case ( p1, p2 ) of
        ( OrganicBlock _, OrganicBlock _ ) ->
            True

        ( ItemBlock _, ItemBlock _ ) ->
            True

        ( FlowerBlock _, FlowerBlock _ ) ->
            True

        _ ->
            [ ( Pickaxe, Rock )
            , ( Fish2, Fish1 )
            , ( Tree1, Tree2 )
            ]
                |> List.any
                    (\pair ->
                        ( p1, p2 ) == pair || ( p2, p1 ) == pair
                    )


isOptional : Block -> Bool
isOptional block =
    case block of
        ItemBlock _ ->
            True

        _ ->
            False
