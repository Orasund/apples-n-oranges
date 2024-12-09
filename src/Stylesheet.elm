module Stylesheet exposing (..)

import Html exposing (Html)
import View.Color


stylesheet : Html msg
stylesheet =
    """
    @font-face {
        font-family: "NotoEmoji";
        src: url("fonts/NotoEmoji-Regular.ttf");
    }

    @font-face {
        font-family: "NotoColorEmoji";
        src: url("fonts/NotoColorEmoji-Regular.ttf");
    }

    .blackWhiteEmoji {
        font-family: serif, "NotoEmoji";
    }

    html {
        height: 100%
    }

    body {
        height: 100%;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        font-family: serif, "NotoColorEmoji";
        
    }

    @keyframes rocking {
        from { rotate:-15deg }
        50% { rotate:15deg }
        to { rotate:-15deg }
    }

     .day {
        background-color: rgba(171,188,246,1);
    }

    @keyframes toDay {
        from {
             background-color: rgba(96,114,175,1);
        }
        to {
              background-color: rgba(171,188,246,1);
        }
    }

    @keyframes movingBackground {
        from {
            background-position-y: 0px;
        }
        to {
            background-position-y: 200px;
        }
    }

    .button-base {
        border-radius: 20px;
        border-width: 0px;
        padding: 8px 16px;
        height: 40px;
        font-size: 16px;
        display: flex;
        gap: 8px;
        align-items: center;
        font-weight: bold;
        color: white;
        box-sizing: border-box;
        font-family: sans-serif;
        background-color: """
        ++ View.Color.white
        ++ """;
    }

    .button.primary-button {
        color: """
        ++ View.Color.white
        ++ """;
        background-color: """
        ++ View.Color.red600
        ++ """;
    }

    .button.primary-button:hover {
        color: """
        ++ View.Color.white
        ++ """;
        background-color: """
        ++ View.Color.red700
        ++ """;
    }

    .button.primary-button:active {
        color: """
        ++ View.Color.white
        ++ """;
        background-color: """
        ++ View.Color.red800
        ++ """;
    }

    .button {
        color: """
        ++ View.Color.red600
        ++ """;
    }

    .button:hover {
        color: """
        ++ View.Color.black
        ++ """;
        background-color: """
        ++ View.Color.red600
        ++ """;
    }

    .button:active {
        color: """
        ++ View.Color.black
        ++ """;
        background-color: """
        ++ View.Color.red800
        ++ """;
    }
    """
        |> Html.text
        |> List.singleton
        |> Html.node "style" []
