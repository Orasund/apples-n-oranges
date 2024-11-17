module Stylesheet exposing (..)

import Html exposing (Html)


stylesheet : Html msg
stylesheet =
    """
    html {
        height: 100%
    }

    body {
        height: 100%;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        background-color:#a4cc95;
    }

    @keyframes rocking {
        from { rotate:-15deg }
        50% { rotate:15deg }
        to { rotate:-15deg }
    }

    .button {
        border-radius: 16px;
        border: 2px solid black;
        padding: 8px 4px;
        background-color: white;
    }

    .button:hover {
        background-color: rgba(255,255,255,0.9);
    }

    .button:active {
        background-color: rgba(255,255,255,0.7);
    }
    """
        |> Html.text
        |> List.singleton
        |> Html.node "style" []
