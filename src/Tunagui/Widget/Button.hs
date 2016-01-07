module Tunagui.Widget.Button
  (
    Config (..)
  , defaultConfig
  , new
  ) where

import Tunagui.Widget.Prim.Button (Config (..), defaultConfig)
import Tunagui.Operation.Window (newButton)

new = newButton
