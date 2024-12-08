module Event exposing (..)

import Data.Block exposing (Optional)
import Puzzle.Setting exposing (Setting)


type Event
    = WeatherEvent { setting : Setting, reward : Maybe Optional }
