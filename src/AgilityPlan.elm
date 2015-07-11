import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Html.Events
import Hurdle exposing (Hurdle(..))
import StartApp

main = StartApp.start { model = model, view = view, update = Hurdle.update }

model = Hurdle.init
    [ { hurdle = Jump        , pos = { x = 100, y = 100 }, angle = 0 }
    , { hurdle = Jump        , pos = { x = 500, y = 400 }, angle = 0 }
    , { hurdle = TireJump    , pos = { x = 200, y = 100 }, angle = 0 }
    , { hurdle = TireJump    , pos = { x = 100, y = 250 }, angle = 45 }
    , { hurdle = WeavePoles 9, pos = { x = 500, y = 200 }, angle = 25 }
    ]

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


view : Signal.Address Hurdle.Action -> Hurdle.Model -> Html
view addr model =
    Html.div []
    [ svg [ version "1.1", x "0", y "0", width "800", height "500", viewBox "0 0 1508 908"
          , preserveAspectRatio "xMidYMid meet" ]
          [ g [ transform "translate(4,4)"]
           ((grid 1500 900 200) :: (Hurdle.view addr model))]
    , Html.div []
        [ Html.button [Html.Events.onClick addr (Hurdle.Add { hurdle = Jump, pos = { x = 300, y = 400 }, angle = 0 })]
                      [Html.text "hello"]
        ]
    ]
