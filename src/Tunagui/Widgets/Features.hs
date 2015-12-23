module Tunagui.Widgets.Features where

import           FRP.Sodium

import qualified Tunagui.General.Types as T

class Clickable a where
  onClick :: a -> Event (T.Point Int)
