module Tunagui.General.Data
  (
    TunaContents (..)
  , TunaState (..)
  , FrameEvents (..)
  , Settings (..)
  --
  , TWindow (..)
  , withTWindow
  --
  ) where

import           Control.Exception      (bracket)
import qualified Data.Text              as T
import           FRP.Sodium
import           GHC.Conc.Sync          (TVar, atomically, newTVar)
import           Linear                 (V2 (..))
import qualified SDL

import           Tunagui.General.Types  (IPoint)
import           Tunagui.Widgets.Layout (Direction (..), WidgetTree (..))

data TunaContents = TunaContents
  { cntTWindow :: TWindow
  , cntEvents  :: FrameEvents
  }

data TunaState = TunaState

data FrameEvents = FrameEvents
  { eQuit :: Event ()
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

newTWindow :: IO TWindow
newTWindow = do
  w <- SDL.createWindow (T.pack "title") winConf
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer
  tree <- atomically . newTVar $ Container DirV []
  return $ TWindow w r tree
  where
    winConf = SDL.defaultWindow
      { SDL.windowResizable = True
      , SDL.windowInitialSize = V2 300 300
      }

freeTWindow :: TWindow -> IO ()
freeTWindow twin = do
  SDL.destroyRenderer $ twRenderer twin
  SDL.destroyWindow $ twWindow twin

withTWindow :: (TWindow -> IO a) -> IO a
withTWindow =
  bracket newTWindow freeTWindow
