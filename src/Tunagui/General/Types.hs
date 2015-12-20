{-# LANGUAGE DeriveFunctor #-}

module Tunagui.General.Types
  (
    IPoint, Point(..)
  ) where

import Linear (V2(..))

type IPoint = Point Int

newtype Point a = P (V2 a)
  deriving (Show, Eq, Functor)
