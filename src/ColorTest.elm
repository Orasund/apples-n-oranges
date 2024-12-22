module ColorTest exposing (..)

import Html exposing (Html)
import Html.Style
import View.Color


main : Html msg
main =
    [ [ View.Color.red100
      , View.Color.red200
      , View.Color.red300
      , View.Color.gray400
      , View.Color.gray500
      , View.Color.red600
      , View.Color.red700
      , View.Color.red800
      , View.Color.red900
      ]
    , [ View.Color.gray100
      , View.Color.brown200
      , View.Color.brown300
      , View.Color.gray400
      , View.Color.gray500
      , View.Color.gray600
      , View.Color.gray700
      , View.Color.gray800
      , View.Color.gray900
      ]
    , [ View.Color.gray100
      , View.Color.gray200
      , View.Color.yellow300
      , View.Color.gray400
      , View.Color.gray500
      , View.Color.gray600
      , View.Color.gray700
      , View.Color.gray800
      , View.Color.gray900
      ]
    , [ View.Color.green100
      , View.Color.green200
      , View.Color.green300
      , View.Color.gray400
      , View.Color.green500
      , View.Color.gray600
      , View.Color.green700
      , View.Color.gray800
      , View.Color.gray900
      ]
    , [ View.Color.blue100
      , View.Color.blue200
      , View.Color.blue300
      , View.Color.gray400
      , View.Color.gray500
      , View.Color.gray600
      , View.Color.gray700
      , View.Color.gray800
      , View.Color.blue900
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
                                [ Html.Style.widthPx 80
                                , Html.Style.heightPx 80
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
