{-# LANGUAGE DeriveFunctor #-}

module Tunagui.General.Types
  (
    Point(..), Size(..)
  , plusPS
  , Range(..)
  , Shape(..), within
  , WidgetId
  , UpdateType (..)
  ) where

import           Linear.V2

newtype Point a = P (V2 a)
  deriving (Show, Eq, Functor)

newtype Size a = S (V2 a)
  deriving (Show, Eq, Functor)

plusPS :: Num a => Point a -> Size a -> Point a
plusPS (P p) (S s) = P $ (+) <$> p <*> s

data Range a = R
  { leftTop     :: Point a
  , rightBottom :: Point a
  } deriving (Show, Eq)

data Shape a
  = Rect (Size a)
  | Circle a

within :: (Num a, Ord a) => Point a -> (Point a, Shape a) -> Bool
within (P (V2 x y)) (P (V2 x0 y0), Rect (S (V2 w h))) =
  (x >= x0) && (x <= x1) && (y >= y0) && (y <= y1)
  where
    x1 = x0 + w
    y1 = y0 + h
within (P (V2 x y)) (P (V2 x0 y0), Circle r) =
  distSqr <= r * r
  where
    dx = (x0 + r) - x
    dy = (y0 + r) - y
    distSqr = dx * dx + dy * dy

type WidgetId = Integer

data UpdateType
  = Reshape -- Widget is reshaped. It affects other widgets.
  | Redraw  -- Widget requires only drawing. It doesn't affect other widgets.
  deriving (Eq, Show)
