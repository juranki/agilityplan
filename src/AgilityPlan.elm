import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Html.Events
import Hurdle exposing (Hurdle(..))
import StartApp

main = StartApp.start { model = model, view = view, update = Hurdle.update }

model = Hurdle.init 1500 900
    [ { hurdle = Jump        , pos = { x = 100, y = 100 }, angle = 0 }
    , { hurdle = Jump        , pos = { x = 500, y = 400 }, angle = 0 }
    , { hurdle = TireJump    , pos = { x = 200, y = 100 }, angle = 0 }
    , { hurdle = TireJump    , pos = { x = 100, y = 250 }, angle = 45 }
    , { hurdle = WeavePoles 9, pos = { x = 500, y = 200 }, angle = 25 }
    ]




view : Signal.Address Hurdle.Action -> Hurdle.Model -> Html
view addr model =
    Html.div []
    [ Hurdle.view addr model
    , Html.div []
        [ Html.button [Html.Events.onClick addr (Hurdle.Add { hurdle = Jump, pos = { x = 300, y = 400 }, angle = 0 })]
                      [Html.text "hello"]
        ]
    ]
