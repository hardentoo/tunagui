module Tunagui.General.Data
  (
    TWindow (..)
  , WinEvents (..)
  , WinConfig (..)
  , withTWindow, newTWindow, freeTWindow
  ) where

import           Control.Monad         (void)
import           Control.Exception     (bracket)
import qualified Data.Text             as T
import           FRP.Sodium
import           GHC.Conc.Sync         (TVar, atomically, newTVar, writeTVar)
import           Linear                (V2 (..))
import qualified SDL

import           Tunagui.General.Base  (Tunagui (..), FrameEvents (..))
import qualified Tunagui.General.Types as T
import           Tunagui.Widget.Layout (WidgetTree (..), Direction (..))

-- TWindow
data TWindow = TWindow
  { twWindow     :: SDL.Window
  , twEvents     :: WinEvents
  , twRenderer   :: SDL.Renderer
  , twWidgetTree :: TVar WidgetTree
  }

-- | Events of each TWindow
data WinEvents = WinEvents
  { weClosed :: Event ()
  , wePML :: Event (T.Point Int)
  , weRML :: Event (T.Point Int)
  }

data WinConfig = WinConfig
  {
    winTitle :: T.Text
  , winResizable :: Bool
  , winInitialSize :: V2 Int
  }

-- TODO: Config with initial position, sizse, scalability
newTWindow :: WinConfig -> FrameEvents -> IO TWindow
newTWindow cnf es = do
  w <- SDL.createWindow (winTitle cnf) winConf
  let es = mkEvents w
  tw <- TWindow w es
          <$> SDL.createRenderer w (-1) SDL.defaultRenderer
          <*> atomically (newTVar (Container DirV []))
  _unlisten <- sync $ listen (weClosed es) $ \_ -> freeTWindow tw -- TODO: Thread leak!?
  return tw
  where
    winConf = SDL.defaultWindow
      { SDL.windowResizable = winResizable cnf
      , SDL.windowInitialSize = fromIntegral <$> winInitialSize cnf
      }
    mkEvents win = WinEvents
      { weClosed = void $ matchWindow id $ eWinClosed es
      , wePML = snd <$> matchWindow fst (ePML es)
      , weRML = snd <$> matchWindow fst (eRML es)
      }
      where
        matchWindow f = filterE ((==win) . f)

freeTWindow :: TWindow -> IO ()
freeTWindow twin = do
  SDL.destroyRenderer $ twRenderer twin
  SDL.destroyWindow $ twWindow twin

withTWindow :: WinConfig -> Tunagui -> (TWindow -> IO a) -> IO a
withTWindow cnf t = bracket (newTWindow cnf events) freeTWindow
  where
    events = cntEvents t
