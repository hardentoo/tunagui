module Tunagui.General.Data
  (
    Window (..)
  , WinEvents (..)
  , WinConfig (..)
  , withWindow, newWindow, freeWindow
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
import           Tunagui.General.Layout (WidgetTree (..), Direction (..))

-- Window
data Window = Window
  { wWindow     :: SDL.Window
  , wEvents     :: WinEvents
  , wRenderer   :: SDL.Renderer
  , wWidgetTree :: TVar WidgetTree
  }

-- | Events of each Window
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
newWindow :: WinConfig -> FrameEvents -> IO Window
newWindow cnf es = do
  sWin <- SDL.createWindow (winTitle cnf) winConf
  let es = mkEvents sWin
  win <- Window sWin es
          <$> SDL.createRenderer sWin (-1) SDL.defaultRenderer
          <*> atomically (newTVar (Container DirV []))
  _unlisten <- sync $ listen (weClosed es) $ \_ -> freeWindow win -- TODO: Check if thread leak occurs
  return win
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

freeWindow :: Window -> IO ()
freeWindow w = do
  SDL.destroyRenderer $ wRenderer w
  SDL.destroyWindow $ wWindow w

withWindow :: WinConfig -> Tunagui -> (Window -> IO a) -> IO a
withWindow cnf t = bracket (newWindow cnf events) freeWindow
  where
    events = cntEvents t
