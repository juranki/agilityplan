module FieldView (view) where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Types exposing (..)
import Dict
import Html exposing (Html)

viewGrid: Grid -> Svg
viewGrid grid =
  let
      w = grid.w
      h = grid.h
      d = grid.density
      ww = w - (rem w d)
      hh = h - (rem h d)
      xRange = [1 .. (round ((toFloat ww) / 200))]
      yRange = [1 .. (round ((toFloat hh) / 200))]
      frame = rect [ x "0", y "0", width (toString w), height (toString h), stroke "#000", strokeWidth "3", fill "none"] []
      verticalLines = List.map
          (\xs ->
              let x = toString (xs * d)
              in
                  line [ x1 x, y1 "0", x2 x, y2 (toString h), stroke "#d99" ] [])
          xRange
      horizontalLines = List.map
          (\ys ->
              let y = toString (ys * d)
              in
                  line [ x1 "0", y1 y, x2 (toString w), y2 y, stroke "#d99" ] [])
          yRange
  in
      g [] ([frame] ++ verticalLines ++ horizontalLines)

view: Signal.Address Action -> Model -> Html
view addr model =
    let
        hurdles = Dict.toList model.hurdles
        svgElements = List.map (\(id, hurdle) ->
                        viewPositionedHurdle addr id hurdle) hurdles
        gridElement = viewGrid model.grid
        vb = "0 0 " ++ (toString (model.grid.w + 8)) ++ " " ++ (toString (model.grid.h + 8))
    in
        svg [ version "1.1", x "0", y "0", width "800", height "500", viewBox vb
              , preserveAspectRatio "xMidYMid meet" ]
              [ g [ transform "translate(4,4)"]
               (gridElement :: svgElements)]

simpleLine: Point -> Point -> Svg
simpleLine p1 p2 =
   line [ x1 (toString p1.x), y1 (toString p1.y)
        , x2 (toString p2.x), y2 (toString p2.y)
        , stroke "#000", strokeWidth "5" ] []

simpleCircle: Point -> Float -> Svg
simpleCircle p radius =
   circle [ cx (toString p.x), cy (toString p.y), r (toString radius)
          , stroke "#000", fill "none", strokeWidth "5" ] []

showHurdle: Hurdle -> List Svg
showHurdle hurdle =
   case hurdle of
       Jump ->
           let w = 100 -- hurdle width
               le = 40 -- line end length
           in
               [ simpleLine { x = -le/2, y = -w/2 } { x = le/2, y = -w/2 }
               , simpleLine { x =   0, y = -w/2 } { x =  0, y =  w/2 }
               , simpleLine { x = -le/2, y =  w/2 } { x = le/2, y =  w/2 } ]
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


viewPositionedHurdle: Signal.Address Action -> ID -> PositionedHurdle -> Svg
viewPositionedHurdle addr id pHurdle =
   let
       [sx, sy, sangle] = List.map toString [ pHurdle.pos.x
                                            , pHurdle.pos.y
                                            , pHurdle.angle ]
   in
       g [ transform ("translate(" ++ sx ++ "," ++ sy ++") rotate(" ++ sangle ++ ")")
         , onClick (Signal.message addr (Move id (pHurdle.pos.x + 10) (pHurdle.pos.y + 10))) ]
         (showHurdle pHurdle.hurdle)
