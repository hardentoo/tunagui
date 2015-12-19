module Tunagui.General.Initialize
  ( withTunagui
  ) where

import Control.Exception
import Linear (V2(..))
import qualified Data.Text as T
import Control.Concurrent (forkIO, threadDelay)

import qualified SDL
import SDL (($=))

import Tunagui.General.Data (Contents(..), Settings)

newTunagui :: Settings -> IO Contents
newTunagui _stg = do
  SDL.initializeAll
  w <- SDL.createWindow (T.pack "title") winConf
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer
  return Contents
    { mainWindow = w
    , mainRenderer = r
    }
  where
    winConf = SDL.defaultWindow
      { SDL.windowResizable = True
      , SDL.windowInitialSize = V2 300 300
      }

releaseTunagui :: Contents -> IO ()
releaseTunagui c = do
  SDL.destroyWindow $ mainWindow c
  SDL.destroyRenderer $ mainRenderer c
  SDL.quit

withTunagui :: Settings -> (Contents -> IO a) -> IO a
withTunagui stg work =
  bracket (newTunagui stg) releaseTunagui $ \tunagui -> do
    forkIO $ do
      eventLoop
      releaseTunagui tunagui
    work tunagui

eventLoop :: IO () -- dummy
eventLoop = loop
  where
    loop :: IO ()
    loop = do
      threadDelay 1000000
      loop
