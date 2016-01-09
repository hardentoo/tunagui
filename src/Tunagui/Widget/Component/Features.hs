module Tunagui.Widget.Component.Features where

import           Control.Monad.IO.Class  (MonadIO)
import           FRP.Sodium

import           Tunagui.General.Types (Point (..), Size (..), Range (..))
import           Tunagui.General.Base (TunaguiT)
import           Tunagui.Internal.Render (RenderT)

class Renderable a where
  render :: a -> RenderT ()
  locate :: a -> Point Int -> IO ()
  size   :: a -> IO (Size Int)
  update :: a -> Event () -- When content is changed
  resize :: a -> Event (Size Int) -- When range is changed
  free   :: a -> IO ()

class Clickable a where
  clickEvent :: a -> Event (Point Int)
