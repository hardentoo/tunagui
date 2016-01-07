module Tunagui.Widget.Label
  (
    Config (..), defaultConfig
  , newT, newB
  ) where

import Tunagui.Widget.Prim.Label (Config (..), defaultConfig)
import Tunagui.Operation.Window (newLabelT, newLabelB)

newT = newLabelT
newB = newLabelB
