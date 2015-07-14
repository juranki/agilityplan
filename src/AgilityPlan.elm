import PlanModel
import Types exposing (Hurdle(..), Model)
import PageView
import StartApp

main =
    let
        model = PlanModel.init 1500 900
            [ { hurdle = Jump        , pos = { x = 100, y = 100 }, angle = 0 }
            , { hurdle = Jump        , pos = { x = 500, y = 400 }, angle = 0 }
            , { hurdle = TireJump    , pos = { x = 200, y = 100 }, angle = 0 }
            , { hurdle = TireJump    , pos = { x = 100, y = 250 }, angle = 45 }
            , { hurdle = WeavePoles 9, pos = { x = 500, y = 200 }, angle = 25 }
            ]
    in
        StartApp.start { model = model, view = PageView.view, update = PlanModel.update }
