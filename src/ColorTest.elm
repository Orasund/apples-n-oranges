module ColorTest exposing (..)

import Html exposing (Html)
import Html.Style
import View.Color


main : Html msg
main =
    [ [ View.Color.gray100
      , View.Color.gray200
      , View.Color.gray300
      , View.Color.gray400
      , View.Color.red500
      , View.Color.red600
      , View.Color.red700
      , View.Color.red800
      , View.Color.gray900
      ]
    , [ View.Color.brown100
      , View.Color.brown200
      , View.Color.gray300
      , View.Color.gray400
      , View.Color.gray500
      , View.Color.gray600
      , View.Color.gray700
      , View.Color.gray800
      , View.Color.gray900
      ]
    , [ View.Color.gray100
      , View.Color.yellow200
      , View.Color.gray300
      , View.Color.gray400
      , View.Color.gray500
      , View.Color.gray600
      , View.Color.gray700
      , View.Color.gray800
      , View.Color.gray900
      ]
    , [ View.Color.green100
      , View.Color.green200
      , View.Color.gray300
      , View.Color.gray400
      , View.Color.green500
      , View.Color.gray600
      , View.Color.green700
      , View.Color.gray800
      , View.Color.gray900
      ]
    , [ View.Color.blue100
      , View.Color.blue200
      , View.Color.gray300
      , View.Color.gray400
      , View.Color.gray500
      , View.Color.gray600
      , View.Color.gray700
      , View.Color.gray800
      , View.Color.gray900
      ]
    , [ View.Color.gray100
      , View.Color.gray200
      , View.Color.gray300
      , View.Color.gray400
      , View.Color.gray500
      , View.Color.gray600
      , View.Color.gray700
      , View.Color.gray800
      , View.Color.gray900
      ]
    ]
        |> List.map
            (\list ->
                list
                    |> List.map
                        (\color ->
                            Html.div
                                [ Html.Style.widthPx 100
                                , Html.Style.heightPx 100
                                , Html.Style.backgroundColor color
                                ]
                                []
                        )
                    |> Html.div
                        [ Html.Style.displayFlex
                        ]
            )
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            ]
