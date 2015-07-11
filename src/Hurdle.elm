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
        TireJump ->
            let
                d = 25  -- tire diameter
                w = 100 -- hurdle width
                le = 40 -- line end length
            in
                [ simpleLine { x = -le/2, y = -w/2 } { x = le/2, y = -w/2 }
                , simpleLine { x = -le/2, y =  w/2 } { x = le/2, y =  w/2 }
                , simpleCircle { x = 0, y = 0 } d
                , simpleLine { x = 0, y = -d } { x = 0, y = -w/2 }
                , simpleLine { x = 0, y = d } { x = 0, y = w/2 } ]
        WeavePoles n ->
            let
                dist = 50
                startPos = (toFloat -(n-1)) * dist / 2
                endPos = -startPos
                poleSticks n =
                    List.map (\i -> simpleLine { x = startPos + ((toFloat i) * dist), y = -5 }
                                               { x = startPos + ((toFloat i) * dist), y = 5 })
                             [0 .. n-1]
            in
                poleSticks n ++ [simpleLine { x = startPos, y = 0 } { x = endPos, y = 0 }]


        _ -> []


showPositionedHurdle: PositionedHurdle -> Svg
showPositionedHurdle pHurdle =
    g [ transform ("translate(" ++ toString pHurdle.pos.x ++ "," ++ toString pHurdle.pos.y
                   ++") rotate(" ++ toString pHurdle.rotation ++ ")") ]
      (showHurdle pHurdle.hurdle)
