module Event exposing (..)

import Puzzle.Setting exposing (Setting)


type Event
    = WeatherEvent Setting
    | ShopEvent
