module Tunagui.General.Initialize
  ( withTunagui
  ) where

import           Control.Concurrent   (forkIO, threadDelay)
import           Control.Exception

import qualified SDL

import           Tunagui.General.Data (Contents(..), Settings, withTWindow)

withTunagui :: Settings -> (Contents -> IO a) -> IO a
withTunagui _stg work =
  bracket SDL.initializeAll
          (const SDL.quit)
          (\_ -> withTWindow $ \twin -> do
                  _ <- forkIO eventLoop
                  work $ Contents twin)

eventLoop :: IO () -- dummy
eventLoop = loop
  where
    loop :: IO ()
    loop = do
      threadDelay 1000000
      loop
