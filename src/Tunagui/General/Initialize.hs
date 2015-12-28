module Tunagui.General.Initialize
  ( withTunagui
  ) where

import           Control.Exception

import qualified SDL
import qualified SDL.Font as TTF

import           Tunagui.General.Data  (Tunagui (..))
import           Tunagui.General.Event (listenAllEvents)

withTunagui :: (Tunagui -> IO a) -> IO a
withTunagui work = bracket_ init quit go
  where
    init = do
      SDL.initializeAll
      TTF.initialize
    quit = do
      TTF.quit
      SDL.quit
    go =
      bracket (TTF.load "data/sample.ttf" 14) TTF.free $ \font -> do
        events <- listenAllEvents
        work $ Tunagui events font
