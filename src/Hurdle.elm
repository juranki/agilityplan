module Hurdle (Hurdle(..), PositionedHurdle, showPositionedHurdle) where

import Svg exposing (..)
import Svg.Attributes exposing (..)

type Hurdle =
    Jump | DoubleJump | TripleJump | PanelJump | LongJump | TireJump
    | Table | WeavePoles Int | AFrame | DogWalk | TeeterTooter
    | Tunnel | CollapsedTunnel

type alias Point = { x: Float, y: Float }

type alias PositionedHurdle = {
    hurdle: Hurdle, pos: Point, rotation: Float
}

simpleLine: Point -> Point -> Svg
simpleLine p1 p2 =
    line [ x1 (toString p1.x), y1 (toString p1.y)
         , x2 (toString p2.x), y2 (toString p2.y)
         , stroke "#000" ] []

simpleCircle: Point -> Float -> Svg
simpleCircle p radius =
    circle [ cx (toString p.x), cy (toString p.y), r (toString radius)
           , stroke "#000", fill "none" ] []

showHurdle: Hurdle -> List Svg
showHurdle hurdle =
    case hurdle of
        Jump -> [ simpleLine { x = -20, y = -50 } { x = 20, y = -50 }
                , simpleLine { x =   0, y = -50 } { x =  0, y =  50 }
                , simpleLine { x = -20, y =  50 } { x = 20, y =  50 } ]
        TireJump -> [ simpleLine { x = -20, y = -50 } { x = 20, y = -50 }
                    , simpleLine { x = -20, y =  50 } { x = 20, y =  50 }
                    , simpleCircle { x = 0, y = 0 } 20
                    , simpleLine { x = 0, y = -20 } { x = 0, y = -50 }
                    , simpleLine { x = 0, y = 20 } { x = 0, y = 50 } ]
        WeavePoles n ->
            (List.map (\i -> simpleLine { x = ((toFloat -(n-1)) * 50 / 2) + ((toFloat i) * 50), y = -5 }
                                  { x = ((toFloat -(n-1)) * 50 / 2) + ((toFloat i) * 50), y = 5 })
                [0 .. n-1])
            ++ [
            simpleLine { x = ((toFloat n-1) * 50 / 2), y = 0 } { x = ((toFloat -(n-1)) * 50 / 2), y = 0 }

        ]
        _ -> []

showPositionedHurdle: PositionedHurdle -> Svg
showPositionedHurdle pHurdle =
    g [ transform ("translate(" ++ toString pHurdle.pos.x ++ "," ++ toString pHurdle.pos.y
                   ++") rotate(" ++ toString pHurdle.rotation ++ ")") ]
      (showHurdle pHurdle.hurdle)
