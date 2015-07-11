import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Hurdle exposing (..)

grid: Int -> Int -> Int -> Svg
grid w h gr =
    let
        ww = w - (rem w gr)
        hh = h - (rem h gr)
        xRange = [1 .. (round ((toFloat ww) / 200))]
        yRange = [1 .. (round ((toFloat hh) / 200))]
        frame = rect [ x "0", y "0", width (toString w), height (toString h), stroke "#000", strokeWidth "3", fill "none"] []
        verticalLines = List.map
            (\xs ->
                let x = toString (xs * gr)
                in
                    line [ x1 x, y1 "0", x2 x, y2 (toString h), stroke "#d99" ] [])
            xRange
        horizontalLines = List.map
            (\ys ->
                let y = toString (ys * gr)
                in
                    line [ x1 "0", y1 y, x2 (toString w), y2 y, stroke "#d99" ] [])
            yRange
    in
        g [] ([frame] ++ verticalLines ++ horizontalLines)

main : Html
main =
  svg [ version "1.1", x "0", y "0", width "100%", height "100%", viewBox "0 0 1508 908"
      , preserveAspectRatio "xMidYMid meet" ]
      [ g [ transform "translate(4,4)"]
       [ grid 1500 900 200
       , showPositionedHurdle { hurdle = Jump, pos = { x = 100, y = 100 }, rotation = 0 }
       , showPositionedHurdle { hurdle = Jump, pos = { x = 500, y = 400 }, rotation = 0 }
       , showPositionedHurdle { hurdle = TireJump, pos = { x = 200, y = 100 }, rotation = 0 }
       , showPositionedHurdle { hurdle = TireJump, pos = { x = 100, y = 250 }, rotation = 45 }
       , showPositionedHurdle { hurdle = WeavePoles 9, pos = { x = 500, y = 200 }, rotation = 25 }
       ]]
