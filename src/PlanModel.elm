module PlanModel (init, update) where

import Dict exposing (Dict)
import Types exposing (..)
import Debug
import Transform2D

init: Float -> Float -> Model
init fieldW fieldH =
    { hurdles = Dict.empty
    , nextId = 1
    , grid = { w = fieldW, h = fieldH, density = 200 }
    , selectedHurdle = Nothing
    }

update: Action -> Model -> Model
update action model =
    case action of
        Add hurdle ->
            let
                center = ( (model.grid.w / 2), (model.grid.h / 2) )
                pHurdle = { hurdle = hurdle
                          , pos = center
                          , angle = 0
                          , t = Transform2D.translation (model.grid.w / 2) (model.grid.h / 2)
                          , t' = Transform2D.translation (-model.grid.w / 2) (-model.grid.h / 2)
                        }
                newId = model.nextId
            in
                { model | hurdles <- Dict.insert model.nextId pHurdle model.hurdles
                        , nextId <- model.nextId + 1 }
                            |> update (SelectHurdle newId)
        Remove id ->
            { model | hurdles <- Dict.remove id model.hurdles }
--        Rotate id deg ->
--            { model | hurdles <-
--                        Dict.update id (\(Just pHurdle) ->
--                                        Just { pHurdle | angle <- deg })
--                                    model.hurdles }
        SelectHurdle id ->
            { model | selectedHurdle <- Just id }
        Move (x, y) ->
            case model.selectedHurdle of
                Just id ->
                    { model | hurdles <-
                        Dict.update id (\(Just pHurdle) ->
                            let
                                nx = toFloat x
                                ny = toFloat y
                            in
                                Just { pHurdle | pos <- (nx, ny)
                                               , t <- Transform2D.multiply
                                                        (Transform2D.translation nx ny)
                                                        (Transform2D.rotation (degrees pHurdle.angle))
                                               , t' <- Transform2D.multiply
                                                        (Transform2D.rotation (degrees -pHurdle.angle))
                                                        (Transform2D.translation -nx -ny)
                                               })
                        model.hurdles}
