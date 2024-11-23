module Event exposing (..)

import Puzzle.Generator exposing (Setting)


type Event
    = WeatherEvent Setting
    | ShopEvent
