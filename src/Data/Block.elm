module Data.Block exposing (..)


type Organic
    = Apple
    | Orange
    | Lemon
    | Carrot
    | Potato
    | Onion


type Flower
    = Sunflower
    | Rose
    | Hyacinth


type Pastries
    = Prezel
    | Crouson
    | Bagle


type Item
    = Coin
    | Wood
    | Stone


type Block
    = OrganicBlock Organic
    | FlowerBlock Flower
    | PastryBlock Pastries
    | ItemBlock Item
    | Fish1
    | Fish2
    | Snow
    | Ice
    | Tree1
    | Tree2
    | Axe


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

        OrganicBlock Onion ->
            "🧅"

        ItemBlock Coin ->
            "🪙"

        ItemBlock Wood ->
            "🪵"

        ItemBlock Stone ->
            "🪨"

        Fish1 ->
            "🐟"

        Fish2 ->
            "🎣"

        FlowerBlock Sunflower ->
            "🌻"

        FlowerBlock Rose ->
            "🌹"

        FlowerBlock Hyacinth ->
            "\u{1FABB}"

        PastryBlock Bagle ->
            "🥯"

        PastryBlock Prezel ->
            "🥨"

        PastryBlock Crouson ->
            "🥐"

        Snow ->
            "❄️"

        Ice ->
            "🧊"

        Tree1 ->
            "🌲"

        Tree2 ->
            "🌳"

        Axe ->
            "🪓"


isValidBlock : Block -> Block -> Bool
isValidBlock p1 p2 =
    case ( p1, p2 ) of
        ( OrganicBlock _, OrganicBlock _ ) ->
            True

        ( ItemBlock _, ItemBlock _ ) ->
            True

        ( FlowerBlock _, FlowerBlock _ ) ->
            True

        ( PastryBlock _, PastryBlock _ ) ->
            True

        _ ->
            [ ( Fish2, Fish1 )
            , ( Snow, Ice )
            , ( Axe, Tree1 )
            , ( Axe, Tree2 )
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
