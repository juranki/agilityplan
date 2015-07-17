import AgilityPlan exposing (Action(..), Model)
import Hurdle exposing (Hurdle(..))
import Window
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Mouse
import Signal
import Transform2D as T2D
import Transform2DApply exposing (applyTransform2D)

model = List.foldl AgilityPlan.update (AgilityPlan.init 2000 1000)
            [ Add Jump
            , Move (100, 100)
            , Add TireJump
            , Move (200, 200)
            , Add (WeavePoles 10)
            , Move (400,300)]

main : Signal Element
main =
    Signal.map2 (view model) Window.dimensions
        (Signal.sampleOn fieldClick.signal
            (Signal.map2
                (\(x,y) t -> applyTransform2D t (toFloat x) (toFloat y))
                Mouse.position fieldClick.signal))

fieldClick : Signal.Mailbox T2D.Transform2D
fieldClick = Signal.mailbox T2D.identity

fieldHover : Signal.Mailbox ()
fieldHover = Signal.mailbox ()

clickTransform : Signal.Mailbox T2D.Transform2D
clickTransform = Signal.mailbox T2D.identity



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

view : Model -> (Int, Int) -> (Float, Float)-> Element
view model (w, h) m =
    let
        pos = show m
    in
        flow down
            [ pos
            , let
                field = AgilityPlan.view model
                (form, t) =  fit model (w, h, (heightOf pos)) field
              in
                clickable (Signal.message fieldClick.address t)
                    (collage w (h - (heightOf pos)) [form])
            ]
