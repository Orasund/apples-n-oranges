module Event exposing (..)

import Puzzle.Setting exposing (Setting)


type Event
    = WeatherEvent { setting : Setting, present : Bool }
    | ShopEvent
