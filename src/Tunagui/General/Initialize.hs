module Tunagui.General.Initialize
  ( withTunagui
  ) where

import           Control.Exception

import qualified Graphics.UI.SDL.TTF   as TTF
import qualified SDL

import           Tunagui.General.Base  (Tunagui (..))
import           Tunagui.General.Event (listenAllEvents)

withTunagui :: (Tunagui -> IO a) -> IO a
withTunagui work =
  bracket_ SDL.initializeAll SDL.quit $
    TTF.withInit $ do
      events <- listenAllEvents
      work $ Tunagui events
