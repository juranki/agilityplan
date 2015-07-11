module Hurdle (Model, Action(..), init, update, view, Hurdle(..), PositionedHurdle) where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Dict exposing (Dict)
import Html exposing (Html)

type Hurdle =
    Jump | DoubleJump | TripleJump | PanelJump | LongJump | TireJump
    | Table | WeavePoles Int | AFrame | DogWalk | TeeterTooter
    | Tunnel | CollapsedTunnel

type alias Point = { x: Float, y: Float }

type alias PositionedHurdle = {
    hurdle: Hurdle, pos: Point, angle: Float
}

type alias ID = Int
type alias Grid = { w: Int, h: Int, density: Int }

type alias Model = {
    hurdles: Dict ID PositionedHurdle,
    nextId: ID,
    grid: Grid
}

type Action =
    Add PositionedHurdle
    | Remove ID
    | Move ID Float Float
    | Rotate ID Float

init: Int -> Int -> List PositionedHurdle -> Model
init fieldW fieldH hurdles =
    { hurdles = Dict.fromList
                    (List.map2 (,) [1 .. (List.length hurdles)] hurdles)
    , nextId = (List.length hurdles) + 1
    , grid = { w = fieldW, h = fieldH, density = 200 }
    }

update: Action -> Model -> Model
update action model =
    case action of
        Add pHurdle ->
            { model | hurdles <- Dict.insert model.nextId pHurdle model.hurdles
                    , nextId <- model.nextId + 1 }
        Remove id ->
            { model | hurdles <- Dict.remove id model.hurdles }
        Move id x y ->
            { model | hurdles <-
                        Dict.update id (\(Just pHurdle) ->
                                        Just { pHurdle | pos <- { x = x, y = y }})
                                    model.hurdles }
        Rotate id deg ->
            { model | hurdles <-
                        Dict.update id (\(Just pHurdle) ->
                                        Just { pHurdle | angle <- deg })
                                    model.hurdles }

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


showPositionedHurdle: Signal.Address Action -> ID -> PositionedHurdle -> Svg
showPositionedHurdle addr id pHurdle =
    g [ transform ("translate(" ++ toString pHurdle.pos.x ++ "," ++ toString pHurdle.pos.y
                   ++") rotate(" ++ toString pHurdle.angle ++ ")")
      , onClick (Signal.message addr (Move id (pHurdle.pos.x + 10) (pHurdle.pos.y + 10))) ]
      (showHurdle pHurdle.hurdle)

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
                        showPositionedHurdle addr id hurdle) hurdles
        gridElement = viewGrid model.grid
        vb = "0 0 " ++ (toString model.grid.w) ++ " " ++ (toString model.grid.h)
    in
        svg [ version "1.1", x "0", y "0", width "800", height "500", viewBox vb
              , preserveAspectRatio "xMidYMid meet" ]
              [ g [ transform "translate(4,4)"]
               (gridElement :: svgElements)]
