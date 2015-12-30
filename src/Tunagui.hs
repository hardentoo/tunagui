module Tunagui
(
  withTunagui
, TunaguiT, runTuna
, withWindow, WinConfig (..)
, WidgetTree (Container)
, Direction (..)
-- * Widgets
, ButtonConfig (..)
, LabelConfig (..)
-- * Features
, onClick
) where

import           Tunagui.General.Data       (withWindow, WinConfig (..))
import           Tunagui.General.Initialize (withTunagui)
import           Tunagui.General.Base       (TunaguiT, runTuna)
import           Tunagui.Widget.Layout      (WidgetTree (..), Direction (..))
import           Tunagui.Widget.Features    (onClick)
import           Tunagui.Widget.Prim.Button (ButtonConfig (..))
import           Tunagui.Widget.Prim.Label  (LabelConfig (..))
