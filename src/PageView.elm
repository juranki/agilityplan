module PageView (view) where

import Types exposing (Hurdle(..), Action(..), Model)
import Html exposing (..)
import Html.Events exposing (..)
import FieldView

view : Signal.Address Action -> Model -> Html
view addr model =
    div []
    [ FieldView.view addr model
    , div []
        [ button [onClick addr (Add Jump)]
                 [text "hello"]
        ]
    ]
