module Tunagui.General.Initialize
  ( withTunagui
  ) where

import           Control.Exception

import qualified SDL

import           Tunagui.General.Data  (Contents (..), Settings, withTWindow)
import           Tunagui.General.Event (listenAllEvents)

withTunagui :: Settings -> (Contents -> IO a) -> IO a
withTunagui _stg work =
  bracket_ SDL.initializeAll SDL.quit $
    withTWindow $ \tWin -> do
      _es <- listenAllEvents
      work $ Contents tWin
