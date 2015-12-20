module Tunagui.General.Types
  ( Point (..)
  ) where

import Linear (V2(..))

newtype Point = P (V2 Int)
