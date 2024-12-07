module Event exposing (..)

import Puzzle.Setting exposing (Setting)


type Event
    = WeatherEvent { setting : Setting }
    | ShopEvent
    | CoinEvent
