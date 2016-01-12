module Tunagui.Widget.Component.Color where

import Data.Word (Word8)
import Linear.V4

type Color = V4 Word8

data ShapeColor = ShapeColor
  { fill :: Color
  , border :: Color
  } deriving (Eq, Show)

planeShapeColor :: ShapeColor
planeShapeColor = ShapeColor
  { fill = V4 70 70 70 255
  , border = V4 120 120 120 255
  }

hoverShapeColor :: ShapeColor
hoverShapeColor = ShapeColor
  { fill = V4 90 90 90 255
  , border = V4 120 120 120 255
  }
