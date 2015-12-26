module Tunagui.General.Data
  (
    TunaContents (..)
  , TunaState (..)
  , FrameEvents (..)
  , Settings (..)
  --
  , TWindow (..)
  , withTWindow, newTWindow, freeTWindow
  --
  ) where

import           Control.Exception     (bracket)
import qualified Data.Text             as T
import           FRP.Sodium
import           GHC.Conc.Sync         (TVar, atomically, newTVar)
import           Linear                (V2 (..))
import qualified SDL

import           Tunagui.General.Types (IPoint)
import           Tunagui.Widget.Layout (WidgetTree (..))

data TunaContents = TunaContents
  { cntEvents  :: FrameEvents
  }

data TunaState = TunaState

-- TODO: Click event must have window ID
data FrameEvents = FrameEvents
  { behQuit :: Behavior Bool
  , ePML  :: Event IPoint -- Press Mouse Left
  , eRML  :: Event IPoint -- Release Mouse Left
  }

data Settings = Settings -- dummy

-- TWindow
data TWindow = TWindow
  { twWindow     :: SDL.Window
  , twRenderer   :: SDL.Renderer
  , twWidgetTree :: TVar WidgetTree
  }

-- newTWindow' :: IO TWindow
-- newTWindow' = newTWindow $ Container DirV []

newTWindow :: WidgetTree -> IO TWindow
newTWindow tree = do
  w <- SDL.createWindow (T.pack "title") temporalWinConf
  TWindow w <$> SDL.createRenderer w (-1) SDL.defaultRenderer
            <*> atomically (newTVar tree)
  where
    temporalWinConf = SDL.defaultWindow
      { SDL.windowResizable = True
      , SDL.windowInitialSize = V2 300 300
      }

freeTWindow :: TWindow -> IO ()
freeTWindow twin = do
  SDL.destroyRenderer $ twRenderer twin
  SDL.destroyWindow $ twWindow twin

-- withTWindow :: (TWindow -> IO a) -> IO a
-- withTWindow =
--   bracket newTWindow' freeTWindow

withTWindow :: WidgetTree -> (TWindow -> IO a) -> IO a
withTWindow tree = bracket (newTWindow tree) freeTWindow
