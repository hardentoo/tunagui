module Tunagui.Widget.Features where

import           Control.Monad.IO.Class  (MonadIO)
import           FRP.Sodium

import qualified Tunagui.General.Types   as T
import           Tunagui.General.Base (TunaguiT)
import           Tunagui.Internal.Render (RenderP)

class Renderable a where
  render :: a -> RenderP TunaguiT ()
  locate :: a -> T.Point Int -> Reactive (T.Range Int)

class Clickable a where
  onClick :: a -> Event (T.Point Int)
