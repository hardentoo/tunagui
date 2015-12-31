module Tunagui.Widget.Component.Util
  ( up, up'
  ) where

import Control.Monad (void)
import FRP.Sodium

up :: Behavior a -> Event String
up = up' ""

up' :: String -> Behavior a -> Event String
up' str beh = const str <$> updates beh
