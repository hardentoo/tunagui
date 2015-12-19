module Tunagui.General.Data
  ( Contents(..)
  , Settings(..)
  ) where

import qualified SDL

data Contents = Contents
  { mainWindow :: SDL.Window
  }

data Settings = Settings -- dummy
