module Tunagui.General.Initialize
  ( withTunagui
  ) where

import           Control.Exception

import qualified SDL

import           Tunagui.General.Data  (Settings, TunaContents (..),
                                        TunaState (..), withTWindow)
import           Tunagui.General.Event (listenAllEvents)
import           Tunagui.Internal.Base (Base, runBase)
import           Tunagui.Operation     (TunaguiP, interpret)

withTunagui :: Settings -> TunaguiP Base a -> IO a
withTunagui _stg pgm =
  bracket_ SDL.initializeAll
           SDL.quit
           (withTWindow $ \tWin -> do
              es <- listenAllEvents
              let cnt = TunaContents tWin es
              fst <$> runBase (interpret pgm) cnt TunaState)
