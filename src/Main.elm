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
    { plan : AgilityPlan.Model
    , windowSize : (Int, Int)
    , t : T2D.Transform2D
    , t' : T2D.Transform2D
    , dropdown : Maybe (Int, Int)
    , drag : Maybe (Float, Float)
    }

type WindowAction = WindowSize (Int, Int)
                  | Click (Int, Int)
                  | Arrow { x:Int, y:Int }
                  | Keypress String
                  | AddHurdle Hurdle (Int, Int)
                  | DragStartStop Bool (Int, Int)
                  | Drag (Int, Int)

init : WindowModel
init =
    { plan = List.foldl AgilityPlan.update (AgilityPlan.init 2000 1000)
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
    , drag = Nothing
    }

update : WindowAction -> WindowModel -> WindowModel
update action model =
    case action of
        WindowSize s ->
            let
                (t, t') = fitWindowTransforms model.plan s
            in
                { model | windowSize <- s
                        , t <- t
                        , t' <- t' }
        Drag (x,y) ->
            let
                (xf, yf) = applyTransform2D model.t' (toFloat x) (toFloat y)
            in
                case model.drag of
                    Just (xf', yf') ->
                        { model | plan <- AgilityPlan.update (AgilityPlan.Move (xf - xf', yf - yf'))
                                                             model.plan}
                    _ -> model
        DragStartStop isdown (x, y) ->
            let
                p = applyTransform2D model.t' (toFloat x) (toFloat y)
            in
                case isdown of
                    True ->
                        let
                            notNothing = (\m -> case m of
                                                    Nothing -> False
                                                    _ -> True)

                            hurdle =
                                List.head
                                    (List.filter notNothing
                                        (List.map (PositionedHurdle.hitTest p)
                                            (Dict.toList model.plan.hurdles)))
                        in
                        --    model
                            case hurdle of
                                Nothing -> { model | dropdown <- case model.dropdown of
                                                                    Nothing -> Just (x,y)
                                                                    _ -> model.dropdown }
                                --_ -> model
                                Just (Just (id, x', y')) -> --model
                                    { model | plan <- AgilityPlan.update (AgilityPlan.SelectHurdle id)
                                                                         model.plan
                                            , dropdown <- Nothing
                                            , drag <- Just (x', y') }
                    False ->
                        case model.drag of
                            Just _ ->  { model | drag <- Nothing }
                            _ -> model

        Arrow direction ->
            { model | plan <- AgilityPlan.update
                                (AgilityPlan.Rotate (-2 * (toFloat direction.x)))
                                model.plan }
        Keypress s ->
            {model | plan <-
                if  | s == "'z'" -> AgilityPlan.update (AgilityPlan.Rotate 10) model.plan
                    | s == "'x'" -> AgilityPlan.update (AgilityPlan.Rotate -10) model.plan
                    | s == "'d'" -> AgilityPlan.update (AgilityPlan.Remove) model.plan
                    | otherwise -> model.plan
                }
        AddHurdle hurdle (x,y) ->
            let
                [x',y'] = List.map toFloat [x,y]
                p = applyTransform2D model.t' x' y'
            in
                { model | plan <- List.foldl AgilityPlan.update model.plan
                            [ AgilityPlan.Add hurdle
                            , AgilityPlan.Move p ]
                        , dropdown <- Nothing }
        _ -> model

model = List.foldl AgilityPlan.update (AgilityPlan.init 2000 1000)
            [ Add Jump
            , Move (100, 100)
            , Add TireJump
            , Move (200, 200)
            , Add (WeavePoles 10)
            , Move (400,300)]

windowSignals =
    Signal.map (\w -> WindowSize w) Window.dimensions

dragMove =
    let
        downup = Signal.dropRepeats Mouse.isDown
    in
        Signal.map (\(isdown, p) -> Drag p)
            (Signal.filter (\(isdown, p) -> isdown) (False, (0,0))
                (Signal.map2 (,) downup Mouse.position))

dragStartStop =
    let
        downup = Signal.dropRepeats Mouse.isDown
    in
        Signal.map (\(isdown, p) -> DragStartStop isdown p)
            (Signal.sampleOn downup
                (Signal.map2 (,) downup Mouse.position))

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
                                   , winAction.signal
                                   , dragStartStop
                                   , dragMove ]
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
        model = (Debug.watch "model" wm).plan
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
                collage w h [form]
            ] ++ dt)
