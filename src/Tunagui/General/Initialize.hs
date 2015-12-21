module Tunagui.General.Initialize
  ( withTunagui
  ) where

import           Control.Applicative
import           Control.Exception

import qualified SDL

import           Tunagui.General.Data  (Settings, TunaContents (..),
                                        withTWindow)
import           Tunagui.General.Event (listenAllEvents)
import           Tunagui.Internal.Base (Base, runBase)
import           Tunagui.Operation     (interpret, TunaguiP)

withTunagui :: Settings -> TunaguiP Base a -> IO a
withTunagui _stg program =
  bracket_ SDL.initializeAll SDL.quit $
    withTWindow $ \tWin -> do
      tunaContents <- TunaContents tWin <$> listenAllEvents
      fst <$> runBase (interpret program) tunaContents
