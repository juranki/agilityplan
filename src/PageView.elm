module PageView (view) where

import Types exposing (Hurdle(..), Action(..), Model)
import Html exposing (..)
import Html.Events exposing (..)
import FieldView

viewSelection : Signal.Address Action -> Model -> Html
viewSelection addr model =
    case model.selectedHurdle of
        Nothing -> span [] [text "no selection"]
        Just id -> span [] [text ((toString id) ++ " selected")]

view : Signal.Address Action -> Model -> Html
view addr model =
    div []
    [ FieldView.view addr model
    , div []
        [ button [onClick addr (Add Jump)]
                 [text "Jump"]
         , button [onClick addr (Add TireJump)]
                  [text "Tire Jump"]
         , button [onClick addr (Add (WeavePoles 10))]
                  [text "Weave Poles 10 poles)"]
        ]
    , div []
        [viewSelection addr model]
    ]
