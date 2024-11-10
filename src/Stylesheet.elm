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
        background-color: transparent;
    }

    .button:hover {
        background-color: rgba(0,0,0,0.1);
    }

    .button:active {
        background-color: rgba(0,0,0,0.3);
    }
    """
        |> Html.text
        |> List.singleton
        |> Html.node "style" []
