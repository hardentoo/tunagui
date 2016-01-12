module Tunagui.Widget.Component.Features where

import           FRP.Sodium

import           Tunagui.General.Types (Point (..), Size (..))
import           Tunagui.Internal.Render (RenderT)

class Renderable a where
  render :: a -> RenderT ()
  locate :: a -> Point Int -> IO ()
  sizeof :: a -> IO (Size Int)
  updated :: a -> Event () -- When content is changed
  resized :: a -> Event (Size Int) -- When range is changed
  free :: a -> IO ()

class Clickable a where
  clickEvent :: a -> Event (Point Int)
