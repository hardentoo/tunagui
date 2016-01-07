module Tunagui.Widget.Component.Features where

import           Control.Monad.IO.Class  (MonadIO)
import           FRP.Sodium

import           Tunagui.General.Types (Point (..), Size (..), Range (..), UpdateType (..))
import           Tunagui.General.Base (TunaguiT)
import           Tunagui.Internal.Render (RenderP)

class Renderable a where
  render :: a -> RenderP TunaguiT ()
  locate :: a -> Point Int -> IO ()
  range  :: a -> IO (Range Int)
  update :: a -> Event UpdateType
  resize :: a -> Event (Size Int)
  free   :: a -> IO ()

class Clickable a where
  clickEvent :: a -> Event (Point Int)
