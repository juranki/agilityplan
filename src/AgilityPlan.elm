import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Hurdle exposing (..)

main : Html
main =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 2000 2000" ]
    [ showPositionedHurdle { hurdle = Jump, pos = { x = 100, y = 100 }, rotation = 0 }
    , showPositionedHurdle { hurdle = TireJump, pos = { x = 200, y = 100 }, rotation = 0 }
    , showPositionedHurdle { hurdle = TireJump, pos = { x = 100, y = 250 }, rotation = 90 }
    , showPositionedHurdle { hurdle = WeavePoles 9, pos = { x = 500, y = 200 }, rotation = 25 }
    ]
