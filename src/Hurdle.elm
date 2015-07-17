module Hurdle(Hurdle(..), view) where

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
            | Tunnel
            | CollapsedTunnel

view: Hurdle -> List Form
view hurdle =
   case hurdle of
       Jump ->
           let w = 100 -- hurdle width
               le = 40 -- line end length
               paths = [ [ (-le/2, -w/2), (le/2, -w/2) ]
                       , [ (0,     -w/2), (0,    w/2 ) ]
                       , [ (-le/2, w/2 ), (le/2, w/2) ]
               ]
           in
               paths |> List.map path
                     |> List.map (traced {defaultLine | cap <- Padded
                                                      , width <- 4})

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


       _ -> []
