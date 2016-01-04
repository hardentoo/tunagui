module Tunagui.Widget
  (
    onClick
  ) where

import Control.Monad (void)
import Control.Concurrent (forkIO)
import FRP.Sodium

import Tunagui.Widget.Component.Features (Clickable, clickEvent)

onClick :: Clickable a => a -> IO () -> IO ()
onClick a f = sync $
  void $ listen e go
  where
    e = clickEvent a
    go _ = void . forkIO $ f
