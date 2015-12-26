module Tunagui.General.Initialize
  ( withTunagui
  ) where

import           Control.Exception

import qualified SDL

import           Tunagui.General.Data  (Settings, TunaContents (..),
                                        TunaState (..), Tunagui (..))
import           Tunagui.General.Event (listenAllEvents)
-- import           Tunagui.Internal.Base (Base, runBase)
-- import           Tunagui.Operation     (TunaguiP, interpret)

withTunagui :: Settings -> (Tunagui -> IO a) -> IO a
withTunagui _stg work =
  bracket_ SDL.initializeAll
           SDL.quit
           (work =<< Tunagui <$> (TunaContents <$> listenAllEvents) <*> pure TunaState)
