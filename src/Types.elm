module Types (..) where

import Dict exposing (Dict)

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

type alias Point = { x: Float
                   , y: Float }

type alias PositionedHurdle = { hurdle: Hurdle
                              , pos: Point
                              , angle: Float
                              }
type alias ID = Int
type alias Grid = { w: Int
                  , h: Int
                  , density: Int }

type alias Model = { hurdles: Dict ID PositionedHurdle
                   , nextId: ID
                   , grid: Grid
                   }

type Action =  Add PositionedHurdle
            | Remove ID
            | Move ID Float Float
            | Rotate ID Float
