module Tunagui.General.Initialize
  ( withTunagui
  ) where

import           Control.Applicative
import           Control.Exception

import qualified SDL

import           Tunagui.General.Data    (Settings, TunaContents (..),
                                          withTWindow)
import           Tunagui.General.Event   (listenAllEvents)
import           Tunagui.General.Tunagui (Tunagui, runTunagui)

withTunagui :: Settings -> Tunagui a -> IO a
withTunagui _stg tunagui =
  bracket_ SDL.initializeAll SDL.quit $
    withTWindow $ \tWin -> do
      contents <- TunaContents tWin <$> listenAllEvents
      fst <$> runTunagui contents tunagui
