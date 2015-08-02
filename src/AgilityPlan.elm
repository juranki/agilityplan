module AgilityPlan (Action(..), Model, init, update, view) where

import Dict exposing (Dict)
import Transform2D
import Hurdle exposing (Hurdle(..))
import PositionedHurdle exposing (PositionedHurdle)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Debug
import Set

type alias ID = Int
type alias Grid = { w: Float
                  , h: Float
                  , density: Float }

type alias Model = { hurdles: Dict ID PositionedHurdle
                   , nextId: ID
                   , grid: Grid
                   , selectedHurdle: Maybe ID
                   }

type Action = Add Hurdle
            | AddNumber
            | Remove
            | SelectHurdle ID
            | Move (Float, Float)
            | Click (Float, Float)
            | Arrow { x:Int, y:Int }
            | Rotate Float


init: Float -> Float -> Model
init fieldW fieldH =
    { hurdles = Dict.empty
    , nextId = 1
    , grid = { w = fieldW, h = fieldH, density = 200 }
    , selectedHurdle = Nothing
    }

firstFreeNum : Set.Set Int -> Int -> Int
firstFreeNum nums candidate =
    if  | (Set.member candidate nums) -> firstFreeNum nums (candidate + 1)
        | otherwise -> candidate

update: Action -> Model -> Model
update action model =
    case action of
        AddNumber ->
            let
                nums = Set.fromList
                        (List.filterMap (\phurdle -> case phurdle.hurdle of
                                                Number n  -> Just n
                                                _ -> Nothing) (Dict.values model.hurdles))
            in
                update (Add (Number (firstFreeNum nums 1))) model
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
        Remove ->
            case model.selectedHurdle of
                Just id -> { model | hurdles <- Dict.remove id model.hurdles
                                   , selectedHurdle <- Nothing}
                Nothing -> model
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
                                nx =  x
                                ny =  y
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
                Nothing -> model
        Rotate diff ->
            case model.selectedHurdle of
                Just id ->
                    { model | hurdles <-
                        Dict.update id (\(Just pHurdle) ->
                            let
                                angle =  pHurdle.angle + diff
                                (nx, ny) = pHurdle.pos
                            in
                                Just { pHurdle | angle <- angle
                                               , t <- Transform2D.multiply
                                                        (Transform2D.translation nx ny)
                                                        (Transform2D.rotation (degrees angle))
                                               , t' <- Transform2D.multiply
                                                        (Transform2D.rotation (degrees -angle))
                                                        (Transform2D.translation -nx -ny)
                                               })
                        model.hurdles}
                Nothing -> model
        Arrow d ->
            case model.selectedHurdle of
                Just id ->
                    let
                        (Just h) = Dict.get id model.hurdles
                        (x,y) = h.pos
                    in
                        model |> update (Move ( x + (toFloat d.x) * 10, y + (toFloat d.y) * 10 ))
                Nothing -> model



grid : Float -> Float -> Float -> List Form
grid w h d =
    let
        xRange = List.map toFloat [0 .. ((round w) // (round d))]
        yRange = List.map toFloat [0 .. ((round h) // (round d))]
        verticalPaths = List.map (\xs -> [ (xs * d, 0), (xs * d, h) ]) xRange
        horizontalPaths = List.map (\ys -> [ (0, ys *d), (w, ys * d) ]) yRange
        ends = [ [ (w,0), (w,h) ], [ (0,h), (w,h) ] ]
    in
        verticalPaths ++ horizontalPaths ++ ends
            |> List.map path
            |> List.map (traced (solid black))

view: Model -> List Form
view model =
    let
        hurdles = Dict.toList model.hurdles
        gridforms = grid model.grid.w model.grid.h model.grid.density
        hurdleforms = List.map (\(_, hurdle) ->
                        PositionedHurdle.view hurdle) hurdles
    in
        gridforms ++ hurdleforms
