module Tunagui.Widget.Component.Util
  ( up
  ) where

import Control.Monad (void)
import FRP.Sodium

up :: Behavior a -> Event ()
up = void . value
