module Tunagui.Widget
  (
    mkButton
  ) where

import Tunagui.Widget.Prim.Button
import qualified Tunagui.Widget.Prim.Button as BTN

mkButton :: BTN.ButtonConfig -> Base (BTN.Button, WidgetTree)
mkButton = do
  -- asks 
  genWT <$> BTN.newButton

genWT :: (Show a, Renderable a) => a -> (a, WidgetTree)
genWT a = (a, Widget a)
