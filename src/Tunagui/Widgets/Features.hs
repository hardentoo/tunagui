module Tunagui.Widgets.Features where

import           FRP.Sodium

import qualified Tunagui.General.Types as T

class Renderable a where
  render :: a -> IO ()

class Clickable a where
  onClick :: a -> Event (T.Point Int)
