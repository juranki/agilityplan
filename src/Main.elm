import AgilityPlan exposing (Action(..), Model)
import Hurdle exposing (Hurdle(..))
import PositionedHurdle
import Window
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Mouse
import Keyboard
import Signal exposing ((<~), (~))
import Transform2D as T2D
import Transform2DApply exposing (applyTransform2D)
import Debug
import Dict exposing (Dict)
import Char
import Time

type alias WindowModel =
    { model : AgilityPlan.Model
    , windowSize : (Int, Int)
    , t : T2D.Transform2D
    , t' : T2D.Transform2D
    , dropdown : Maybe (Int, Int)
    }

type WindowAction = WindowSize (Int, Int)
                  | Click (Int, Int)
                  | Arrow { x:Int, y:Int }
                  | Keypress String
                  | AddHurdle Hurdle (Int, Int)

init : WindowModel
init =
    { model = List.foldl AgilityPlan.update (AgilityPlan.init 2000 1000)
                [ Add Jump
                , Move (100, 200)
                , Add TireJump
                , Move (200, 200)
                , Add (WeavePoles 10)
                , Move (400,300)]
    , windowSize = (1000, 1000)
    , t = T2D.identity
    , t' = T2D.identity
    , dropdown = Nothing
    }

update : WindowAction -> WindowModel -> WindowModel
update action model =
    case action of
        WindowSize s ->
            let
                (t, t') = fitWindowTransforms model.model s
            in
                { model | windowSize <- s
                        , t <- t
                        , t' <- t' }
        Click (x, y) ->
            let
                fx = toFloat x
                fy = toFloat y
                p = applyTransform2D model.t' fx fy
                hurdle = List.head
                    (List.filter (PositionedHurdle.hitTest p) (Dict.toList model.model.hurdles))
            in
                case hurdle of
                    Nothing -> { model | dropdown <- Just (x,y) }
                    Just (id, _) ->
                        { model | model <- AgilityPlan.update
                                            (AgilityPlan.SelectHurdle id)
                                            model.model
                                , dropdown <- Nothing }
        Arrow direction ->
            { model | model <- AgilityPlan.update
                                (AgilityPlan.Arrow direction)
                                model.model }
        Keypress s ->
            {model | model <-
                if  | s == "'z'" -> AgilityPlan.update (AgilityPlan.Rotate 10) model.model
                    | s == "'x'" -> AgilityPlan.update (AgilityPlan.Rotate -10) model.model
                    | s == "'d'" -> AgilityPlan.update (AgilityPlan.Remove) model.model
                    | otherwise -> model.model
                }
        AddHurdle hurdle (x,y) ->
            let
                [x',y'] = List.map toFloat [x,y]
                p = applyTransform2D model.t' x' y'
            in
                { model | model <- List.foldl AgilityPlan.update model.model
                            [ AgilityPlan.Add hurdle
                            , AgilityPlan.Move p ]
                        , dropdown <- Nothing }

model = List.foldl AgilityPlan.update (AgilityPlan.init 2000 1000)
            [ Add Jump
            , Move (100, 100)
            , Add TireJump
            , Move (200, 200)
            , Add (WeavePoles 10)
            , Move (400,300)]

windowSignals =
    Signal.map (\w -> WindowSize w) Window.dimensions

arrowSignals =
    Signal.map Arrow
        (Signal.filter (\d -> (d.x /= 0) || (d.y /= 0)) {x=0,y=0}
            (Signal.sampleOn (Time.fps 30) Keyboard.arrows))
keySignals =
    Signal.map (\i -> Debug.watch "key" (Keypress (toString (Char.fromCode i)))) Keyboard.presses

main : Signal Element
main =
    let
        actions = Signal.mergeMany [ arrowSignals
                                   , windowSignals
                                   , fieldClicks
                                   , keySignals
                                   , winAction.signal ]
        updateLoop = Signal.foldp update init actions
    in
        Signal.map view updateLoop

fieldClick : Signal.Mailbox ()
fieldClick = Signal.mailbox ()

winAction : Signal.Mailbox WindowAction
winAction = Signal.mailbox (Keypress "")

fieldClicks =
    Signal.map
        (\p -> Click p)
        (Signal.sampleOn fieldClick.signal Mouse.position)

fitWindowTransforms : AgilityPlan.Model -> (Int,Int) -> (T2D.Transform2D, T2D.Transform2D)
fitWindowTransforms model (w, h) =
    let
        [ fw, fh ] = List.map toFloat [ w, h ]
        scalingFactor = min (fw / (model.grid.w + 20))
                            (fh / (model.grid.h + 20))

        -- t = center and scale model
        tCenter = T2D.translation (-model.grid.w / 2) (-model.grid.h / 2)
        tScale = T2D.scale scalingFactor
        t = T2D.multiply tScale tCenter

        -- t' = mouse coordinates to model coordinates
        elemCenter = T2D.translation (-fw/2) (-fh/2)
        mirrorY = T2D.matrix 1 0 0 -1 0 0
        tScale' = T2D.scale (1 / scalingFactor)
        tCenter' = T2D.translation (model.grid.w / 2) (model.grid.h / 2)
        t' = List.foldr T2D.multiply elemCenter [ tCenter', tScale', mirrorY ]
    in
        (t, t')


fit : Model -> (Int, Int, Int) -> List Form -> (Form, T2D.Transform2D)
fit model (w, h, top) s =
    let
        [ fw, fh, ftop ] = List.map toFloat [ w, h, top ]
        scalingFactor = min (fw / (model.grid.w + 20))
                            ((fh - ftop) / (model.grid.h + 20))

        -- t = center and scale model
        tCenter = T2D.translation (-model.grid.w / 2) (-model.grid.h / 2)
        tScale = T2D.scale scalingFactor
        t = T2D.multiply tScale tCenter

        -- t' = mouse coordinates to model coordinates
        elemCenter = T2D.translation (-fw/2) (-fh/2-ftop/2)
        mirrorY = T2D.matrix 1 0 0 -1 0 0
        tScale' = T2D.scale (1 / scalingFactor)
        tCenter' = T2D.translation (model.grid.w / 2) (model.grid.h / 2)
        t' = List.foldr T2D.multiply elemCenter [ tCenter', tScale', mirrorY ]

    in
        (groupTransform t s, t')

view : WindowModel -> Element
view wm =
    let
        model = wm.model
        pos = show model.selectedHurdle
        (w, h) = wm.windowSize
        dt = case wm.dropdown of
                Just (x,y) -> [ container w h
                                (middleAt (absolute x) (absolute y))
                                (flow down
                                    [ button (Signal.message winAction.address (AddHurdle Jump (x,y))) "Jump"
                                    , button (Signal.message winAction.address (AddHurdle TireJump (x,y))) "Tire jump"
                                    , button (Signal.message winAction.address (AddHurdle (WeavePoles 10) (x,y))) "Weave poles"
                                    , button (Signal.message winAction.address (AddHurdle (Tunnel 450) (x,y))) "Tunnel"
                                    , button (Signal.message winAction.address (AddHurdle (CurvedTunnel 120) (x,y))) "Curved tunnel"
                                    ])]
                Nothing -> []
    in
        layers
            ([ pos
            , let
                hpos = 0 -- heightOf pos
                field = AgilityPlan.view model
                form = groupTransform wm.t field
                --(form, t) =  fit model (w, h, hpos) field
              in
                clickable (Signal.message fieldClick.address ())
                    (collage w h [form])
            ] ++ dt)
