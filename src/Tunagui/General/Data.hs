module Tunagui.General.Data
  (
    Tunagui (..)
  , TunaContents (..)
  , TunaState (..)
  , FrameEvents (..)
  , Settings (..)
  --
  , TWindow (..)
  , WinEvents (..)
  , withTWindow, newTWindow, freeTWindow
  --
  , testOverwriteTree
  ) where

import           Control.Exception     (bracket)
import qualified Data.Text             as T
import           FRP.Sodium
import           GHC.Conc.Sync         (TVar, atomically, newTVar, writeTVar)
import           Linear                (V2 (..))
import qualified SDL

import qualified Tunagui.General.Types as T
import           Tunagui.Widget.Layout (WidgetTree (..), Direction (..))

data Tunagui = Tunagui TunaContents TunaState

data TunaContents = TunaContents
  { cntEvents  :: FrameEvents
  }

data TunaState = TunaState

-- TODO: Click event must have window ID
data FrameEvents = FrameEvents
  { behQuit :: Behavior Bool
  , ePML  :: Event (SDL.Window, T.Point Int) -- Press Mouse Left
  , eRML  :: Event (SDL.Window, T.Point Int) -- Release Mouse Left
  }

data Settings = Settings -- dummy

-- TWindow
data TWindow = TWindow
  { twWindow     :: SDL.Window
  , twRenderer   :: SDL.Renderer
  , twWidgetTree :: TVar WidgetTree
  , twEvents     :: WinEvents
  }

-- | Events of each TWindow
data WinEvents = WinEvents
  { wePML :: Event (T.Point Int)
  , weRML :: Event (T.Point Int)
  }

-- TODO: Config with initial position
-- TODO: Config with initial size
-- TODO: Config with scalability
newTWindow :: FrameEvents -> IO TWindow
newTWindow es = do
  w <- SDL.createWindow (T.pack "title") temporalWinConf
  TWindow w <$> SDL.createRenderer w (-1) SDL.defaultRenderer
            <*> atomically (newTVar (Container DirV []))
            <*> pure (mkEvents w)
  where
    temporalWinConf = SDL.defaultWindow
      { SDL.windowResizable = True
      , SDL.windowInitialSize = V2 300 300
      }
    mkEvents win = WinEvents
        { wePML = matchWindow $ ePML es
        , weRML = matchWindow $ eRML es
        }
      where
        matchWindow = fmap snd . filterE ((==win) . fst)

freeTWindow :: TWindow -> IO ()
freeTWindow twin = do
  SDL.destroyRenderer $ twRenderer twin
  SDL.destroyWindow $ twWindow twin

withTWindow :: FrameEvents -> (TWindow -> IO a) -> IO a
withTWindow fe = bracket (newTWindow fe) freeTWindow

testOverwriteTree :: WidgetTree -> TWindow -> IO ()
testOverwriteTree tree tw =
  atomically $ writeTVar (twWidgetTree tw) tree
