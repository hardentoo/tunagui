module Tunagui.General.Data
  ( Contents(..)
  , FrameEvents(..)
  , Settings(..)
  --
  , TWindow(..)
  -- , newTWindow
  -- , releaseTWindow
  , withTWindow
  --
  ) where

import           Control.Exception (bracket)
import qualified Data.Text         as T
import           Linear            (V2 (..))
import qualified SDL
import           FRP.Sodium

import           Tunagui.General.Types (IPoint)

data Contents = Contents
  { mainWindow :: TWindow
  }

data FrameEvents = FrameEvents
  { eQuit :: Event ()
  , ePML :: Event IPoint -- Press Mouse Left
  , eRML :: Event IPoint -- Release Mouse Left
  }

data Settings = Settings -- dummy

-- TWindow
data TWindow = TWindow
  { twWindow   :: SDL.Window
  , twRenderer :: SDL.Renderer
--, twWidgetTree
  }

newTWindow :: IO TWindow
newTWindow = do
  w <- SDL.createWindow (T.pack "title") winConf
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer
  return $ TWindow w r
  where
    winConf = SDL.defaultWindow
      { SDL.windowResizable = True
      , SDL.windowInitialSize = V2 300 300
      }

releaseTWindow :: TWindow -> IO ()
releaseTWindow twin = do
  SDL.destroyRenderer $ twRenderer twin
  SDL.destroyWindow $ twWindow twin

withTWindow :: (TWindow -> IO a) -> IO a
withTWindow =
  bracket newTWindow releaseTWindow
