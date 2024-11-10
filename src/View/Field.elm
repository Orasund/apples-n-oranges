module View.Field exposing (..)

import Html exposing (Attribute, Html)
import Html.Style


light : Attribute msg
light =
    Html.Style.background "#399E5A"


dark : Attribute msg
dark =
    Html.Style.background "#2D6233"


size : Float
size =
    60



--80


toHtml : List (Attribute msg) -> { rows : Int, columns : Int } -> Html msg
toHtml attrs args =
    List.range 0 (args.rows - 1)
        |> List.map
            (\y ->
                List.range 0 (args.columns - 1)
                    |> List.map
                        (\x ->
                            Html.div
                                [ Html.Style.aspectRatio "1"
                                , Html.Style.widthPx size
                                , if x + y |> modBy 2 |> (==) 0 then
                                    light

                                  else
                                    dark
                                ]
                                []
                        )
                    |> Html.div [ Html.Style.displayFlex, Html.Style.flexDirectionRow ]
            )
        |> Html.div
            ([ Html.Style.displayFlex
             , Html.Style.flexDirectionColumn
             , Html.Style.overflowHidden
             , Html.Style.borderRadiusPx (size / 4)
             , Html.Style.widthMinContent
             ]
                ++ attrs
            )
