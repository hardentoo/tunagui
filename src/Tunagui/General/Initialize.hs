module Tunagui.General.Initialize
  ( withTunagui
  ) where

import           Control.Exception

import qualified SDL
import qualified SDL.Font as TTF

import           Tunagui.General.Base  (Tunagui (..))
import           Tunagui.General.Event (listenAllEvents)

withTunagui :: (Tunagui -> IO a) -> IO a
withTunagui work =
  bracket_ SDL.initializeAll SDL.quit $
    bracket_ TTF.initialize TTF.quit $
      bracket (TTF.load "data/sample.ttf" 16) TTF.free $ \font -> do
        events <- listenAllEvents
        work $ Tunagui events font
