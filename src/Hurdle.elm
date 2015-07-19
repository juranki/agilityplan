module Hurdle(Hurdle(..), view, hitTest) where

import Graphics.Collage exposing (..)

type Hurdle = Jump
            | DoubleJump
            | TripleJump
            | PanelJump
            | LongJump
            | TireJump
            | Table
            | WeavePoles Int
            | AFrame
            | DogWalk
            | TeeterTooter
            | Tunnel Float
            | CurvedTunnel Float
            | CollapsedTunnel

view: Hurdle -> List Form
view hurdle =
    case hurdle of
        Jump ->
            let w = 90 -- hurdle width
                path = traced {defaultLine | cap <- Padded, width <- 4}
                        [ (0, -w/2), (0, w/2 ) ]
                supports = [ rect 10 40,  rect 10 40 ]
                    |> List.map (outlined {defaultLine | cap <- Padded
                                                       , width <- 4})
                supports'  = List.map2 moveY [-65,65] supports
            in
                path :: supports'
        TireJump ->
            let
                d = 25  -- tire diameter
                w = 100 -- hurdle width
                le = 40 -- line end length
                paths = [ [ (-le/2, -w/2), (le/2, -w/2) ]
                        , [ (-le/2,  w/2), (le/2,  w/2) ]
                        , [ (0, -d), (0, -w/2) ]
                        , [ (0,  d), (0,  w/2) ] ]
                shapes = (circle d) :: List.map path paths
            in
                shapes |> List.map (traced {defaultLine | cap <- Padded
                                                        , width <- 4})
        WeavePoles n ->
            let
                dist = 50
                startPos = (toFloat -(n-1)) * dist / 2
                endPos = -startPos
                poleSticks n =
                    List.map (\i -> [ ( startPos + ((toFloat i) * dist), -5 )
                                    , ( startPos + ((toFloat i) * dist),  5 )])
                             [0 .. n-1]
            in
                [ ( startPos, 0 ), (endPos, 0) ] :: (poleSticks n)
                    |> List.map path
                    |> List.map (traced {defaultLine | cap <- Padded
                                                     , width <- 4})
        Tunnel m ->
            let
                w = 60
                startX = -m/2
                endX = -startX
                lines = [ [ (startX,  w/2), (endX,  w/2) ]
                        , [ (startX, -w/2), (endX, -w/2) ]]
                            |> List.map (traced {defaultLine | cap <- Padded
                                                             , width <- 4})
                ovals = [(oval 20 60),(oval 20 60)]
                            |> List.map (outlined {defaultLine | cap <- Padded
                                                               , width <- 4})
                ovals' = List.map2 moveX [-m/2, m/2] ovals

            in
                lines ++ ovals'
        CurvedTunnel r ->
            let
                thetas = List.map (\a -> degrees (a*10)) [0 .. 18]
                innerPath = List.map (circlePoint (r - 30)) thetas
                outerPath = List.map (circlePoint (r + 30)) thetas
                lines = [innerPath, outerPath]
                    |> List.map (traced {defaultLine | cap <- Padded
                                                     , width <- 4})
                ovals = [oval 60 20, oval 60 20]
                    |> List.map (outlined {defaultLine | cap <- Padded
                                                       , width <- 4})
                ovals' = List.map2 moveX [-r, r] ovals
            in
                lines ++ ovals'
        _ -> []


hitTest : (Float, Float) -> Hurdle -> Bool
hitTest (x, y) hurdle =
    case hurdle of
        Jump ->
            ((abs x) < 10) && ((abs y) < 85)
        TireJump ->
            ((abs x) < 20) && ((abs y) < 50)
        WeavePoles n ->
            let
                dist = 50
                startPos = (toFloat (n-1)) * dist / 2
            in
                ((abs x) < startPos) && ((abs y) < 5)
        Tunnel m ->
            ((abs x) < (m/2)) && ((abs y) < 30)
        CurvedTunnel r ->
            let r' = sqrt ((x^2) + (y^2))
            in
                (y >= 0) && ((abs (r - r')) < 30)
        _ -> False

circlePoint : Float -> Float -> (Float, Float)
circlePoint r theta = (r * (cos theta), r * (sin theta))
