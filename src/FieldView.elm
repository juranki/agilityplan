module FieldView (view) where

import Graphics.Collage exposing (..)
import Transform2D as T2D
import Types exposing (..)
import Dict
import PositionedHurdle

view: Model -> List Form
view model =
    let
        hurdles = Dict.toList model.hurdles
    in
        List.map (\(_, hurdle) ->
                        PositionedHurdle.view hurdle) hurdles
