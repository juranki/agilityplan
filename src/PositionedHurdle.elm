module PositionedHurdle(PositionedHurdle, view) where

import Graphics.Collage exposing (..)
import Hurdle exposing (Hurdle)
import Transform2D exposing (Transform2D)

type alias PositionedHurdle = { hurdle: Hurdle
                              , pos: (Float, Float)
                              , angle: Float
                              , t: Transform2D
                              , t': Transform2D
                              }

view: PositionedHurdle -> Form
view  pHurdle =
    groupTransform pHurdle.t ( Hurdle.view pHurdle.hurdle)
