{-# LANGUAGE DeriveFunctor #-}

module Tunagui.General.Types
  (
    Point(..), IPoint
  , Size(..), ISize
  , Shape(..)
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
  | Circle ((Point a) a)
