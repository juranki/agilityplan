import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Html.Events
import Hurdle
import Types exposing (Hurdle(..), Action(..), Model)
import StartApp

main =
    let
        model = Hurdle.init 1500 900
            [ { hurdle = Jump        , pos = { x = 100, y = 100 }, angle = 0 }
            , { hurdle = Jump        , pos = { x = 500, y = 400 }, angle = 0 }
            , { hurdle = TireJump    , pos = { x = 200, y = 100 }, angle = 0 }
            , { hurdle = TireJump    , pos = { x = 100, y = 250 }, angle = 45 }
            , { hurdle = WeavePoles 9, pos = { x = 500, y = 200 }, angle = 25 }
            ]
    in
        StartApp.start { model = model, view = view, update = Hurdle.update }

view : Signal.Address Action -> Model -> Html
view addr model =
    Html.div []
    [ Hurdle.view addr model
    , Html.div []
        [ Html.button [Html.Events.onClick addr (Add { hurdle = Jump, pos = { x = 300, y = 400 }, angle = 0 })]
                      [Html.text "hello"]
        ]
    ]
