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
        
        //from {background: linear-gradient(0deg, rgba(171,188,246,1) 0%, rgba(171,188,246,1) 50%, rgba(144,165,236,1) 50%); }
        //to {background: linear-gradient(180deg, rgba(171,188,246,1) 0%, rgba(171,188,246,1) 50%, rgba(144,165,236,1) 50%);}
    }

    .button {
        border-radius: 16px;
        border: 2px solid black;
        padding: 8px 4px;
        background-color: white;
        border-bottom-width: 6px;
    }

    .button:hover {
        border-bottom-width: 4px;
    }

    .button:active {
        border-bottom-width: 2px;
    }
    """
        |> Html.text
        |> List.singleton
        |> Html.node "style" []
