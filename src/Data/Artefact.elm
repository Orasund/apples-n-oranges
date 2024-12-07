module Data.Artefact exposing (..)


type Artifact
    = Alien
    | Crown
    | Ring
    | Mushroom
    | Egg
    | Bone
    | Compass
    | Coin
    | Diamant
    | Key


list : List Artifact
list =
    [ Alien
    , Crown
    , Ring
    , Mushroom
    , Egg
    , Bone
    , Compass
    , Coin
    , Diamant
    , Key
    ]


toString : Artifact -> String
toString artifact =
    case artifact of
        Alien ->
            "👾"

        Crown ->
            "👑"

        Ring ->
            "💍"

        Mushroom ->
            "🍄"

        Egg ->
            "🥚"

        Bone ->
            "🦴"

        Compass ->
            "🧭"

        Coin ->
            "🪙"

        Diamant ->
            "💎"

        Key ->
            "🗝️"
