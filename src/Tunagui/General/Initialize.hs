module Tunagui.General.Initialize
  ( withTunagui
  ) where

import           Control.Concurrent   (forkIO)
import           Control.Exception
import           Control.Monad        (unless)

import qualified SDL

import           Tunagui.General.Data (Contents (..), Settings, withTWindow)

withTunagui :: Settings -> (Contents -> IO a) -> IO a
withTunagui _stg work =
  bracket_ SDL.initializeAll SDL.quit $
    withTWindow $ \tWin -> do
      _ <- forkIO eventLoop
      work $ Contents tWin

eventLoop :: IO () -- MOVE!
eventLoop = loop
  where
    loop :: IO ()
    loop = do
      es <- SDL.pollEvents
      unless (any isQuit es) loop

    isQuit :: SDL.Event -> Bool
    isQuit e = SDL.eventPayload e == SDL.QuitEvent
