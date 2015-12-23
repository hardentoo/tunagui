module Tunagui.Widgets.Prim.Component.Clickable where
  -- (
  -- ) where

import FRP.Sodium

import qualified Tunagui.General.Types as T

data Clickable

clickEvent :: IO (Event (T.Point Int))
clickEvent = undefined

-- mkClickable :: Behavior T.Shape -> IO Clickable
-- mkClickable beh = do
  -- * need click event of mouse!
