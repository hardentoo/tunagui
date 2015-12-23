{-# LANGUAGE DeriveFunctor #-}

module Tunagui.General.Types
  (
    Point(..), IPoint
  , Size(..), ISize
  , Shape(..), within
  ) where

import           Linear (V2 (..))

newtype Point a = P (V2 a)
  deriving (Show, Eq, Functor)

newtype Size a = S (V2 a)
  deriving (Show, Eq, Functor)

type IPoint = Point Int
type ISize = Size Int

data Shape a
  = Rect (Point a) (Size a)
  | Circle (Point a) a

within :: (Num a, Ord a) => Point a -> Shape a -> Bool
within (P (V2 x y)) (Rect (P (V2 x1 y1)) (S (V2 w h))) =
  (x >= x1) && (x <= x2) && (y >= y1) && (y <= y2)
  where
    x2 = x1 + w
    y2 = y1 + h
within (P (V2 x y)) (Circle (P (V2 x0 y0)) r) =
  distSqr <= r * r
  where
    dx = x0 - x
    dy = y0 - y
    distSqr = dx * dx + dy * dy
