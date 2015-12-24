module Tunagui.Widget.Features where

import           Control.Monad.IO.Class            (MonadIO)
import           FRP.Sodium

import qualified Tunagui.General.Types             as T
import           Tunagui.Internal.Operation.Render

class Renderable a where
  render :: MonadIO m => a -> RenderP m ()

class Clickable a where
  onClick :: a -> Event (T.Point Int)
