{-# LANGUAGE ExistentialQuantification #-}

module Tunagui.Widgets.Layout where

import Tunagui.Widgets.Features (Renderable, render)

data WidgetTree =
  forall a. (Renderable a)
  => Widget a | Container Direction [WidgetTree]

data Direction
  = DirH -- Horizontal
  | DirV -- Vertical
  deriving Show
