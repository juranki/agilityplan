module PlanModel (init, update) where

import Dict exposing (Dict)
import Types exposing (..)

init: Int -> Int -> List PositionedHurdle -> Model
init fieldW fieldH hurdles =
    { hurdles = Dict.fromList
                    (List.map2 (,) [1 .. (List.length hurdles)] hurdles)
    , nextId = (List.length hurdles) + 1
    , grid = { w = fieldW, h = fieldH, density = 200 }
    }

update: Action -> Model -> Model
update action model =
    case action of
        Add pHurdle ->
            { model | hurdles <- Dict.insert model.nextId pHurdle model.hurdles
                    , nextId <- model.nextId + 1 }
        Remove id ->
            { model | hurdles <- Dict.remove id model.hurdles }
        Move id x y ->
            { model | hurdles <-
                        Dict.update id (\(Just pHurdle) ->
                                        Just { pHurdle | pos <- { x = x, y = y }})
                                    model.hurdles }
        Rotate id deg ->
            { model | hurdles <-
                        Dict.update id (\(Just pHurdle) ->
                                        Just { pHurdle | angle <- deg })
                                    model.hurdles }
