module Transform2DApply(applyTransform2D) where

import Native.Transform2DApply
import Transform2D as T2D

applyTransform2D: T2D.Transform2D -> Float -> Float -> (Float, Float)
applyTransform2D = Native.Transform2DApply.apply
