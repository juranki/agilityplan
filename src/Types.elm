module Types (..) where

import Hurdle exposing (Hurdle)
import PositionedHurdle exposing (PositionedHurdle)
import Dict exposing (Dict)

type alias ID = Int
type alias Grid = { w: Float
                  , h: Float
                  , density: Float }

type alias Model = { hurdles: Dict ID PositionedHurdle
                   , nextId: ID
                   , grid: Grid
                   , selectedHurdle: Maybe ID
                   }

type Action = Add Hurdle
            | Remove ID
            | SelectHurdle ID
            | Move (Int, Int)
