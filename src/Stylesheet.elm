module Stylesheet exposing (..)

import Html exposing (Html)


stylesheet : Html msg
stylesheet =
    """
    @keyframes rocking {
        from { rotate:-15deg }
        50% { rotate:15deg }
        to { rotate:-15deg }
    }
    """
        |> Html.text
        |> List.singleton
        |> Html.node "style" []
