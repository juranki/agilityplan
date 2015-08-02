module PositionedHurdle(PositionedHurdle, view, hitTest) where

import Graphics.Collage exposing (..)
import Hurdle exposing (Hurdle)
import Transform2D exposing (Transform2D)
import Transform2DApply exposing (applyTransform2D)

type alias PositionedHurdle = { hurdle: Hurdle
                              , pos: (Float, Float)
                              , angle: Float
                              , t: Transform2D
                              , t': Transform2D
                              }

view: PositionedHurdle -> Form
view  pHurdle =
    groupTransform pHurdle.t ( Hurdle.view pHurdle.hurdle)

hitTest : (Float, Float) -> (Int, PositionedHurdle) -> Maybe (Int, Float, Float)
hitTest (x, y) (id, hurdle) =
    let
        p = applyTransform2D hurdle.t' x y
        isHit = Hurdle.hitTest p hurdle.hurdle
        (x', y') = hurdle.pos
    in
        if  | isHit -> Just (id, x - x', y - y')
            | otherwise -> Nothing
