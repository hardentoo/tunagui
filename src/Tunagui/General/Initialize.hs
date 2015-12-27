module Tunagui.General.Initialize
  ( withTunagui
  ) where

import           Control.Exception

import qualified SDL
import qualified SDL.Font as TTF

import           Tunagui.General.Data  (Settings, TunaContents (..),
                                        TunaState (..), Tunagui (..))
import           Tunagui.General.Event (listenAllEvents)

withTunagui :: Settings -> (Tunagui -> IO a) -> IO a
withTunagui _stg work =
  bracket_ init
           quit
           (work =<< Tunagui <$> (TunaContents <$> listenAllEvents) <*> pure TunaState)
  where
    init = do
      SDL.initializeAll
      TTF.initialize
    quit = do
      TTF.quit
      SDL.quit
