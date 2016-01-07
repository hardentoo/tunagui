{-# LANGUAGE ExistentialQuantification #-}

module Tunagui.General.Data
  (
    Window (..)
  , WinEvents (..)
  , WinConfig (..)
  , withWindow, newWindow, freeWindow
  -- *Layout
  , WidgetTree (..)
  , Direction (..)
  , newWidget
  , locateWT
  , renderWT
  , mkUpdateEventWT
  , DimSize (..)
  ) where

import           Control.Monad         (void, foldM, when)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Trans.State
import           Data.List               (foldl', foldl1')
import           Control.Exception     (bracket, bracket_)
import qualified Data.Text             as T
import           FRP.Sodium
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TMVar
import           GHC.Conc.Sync
import           Linear.V2
import           Linear.V4
import qualified SDL
import           Data.Set              (Set)
import qualified Data.Set              as Set

import           Tunagui.General.Base  (Tunagui (..), FrameEvents (..), TunaguiT)
import qualified Tunagui.General.Types as T
import           Tunagui.Widget.Component.Features (Renderable, locate, range, render, update, free)
import           Tunagui.Internal.Render (RenderP)

-- Window
data Window = Window
  { wWindow     :: SDL.Window
  , wEvents     :: WinEvents
  , wRenderer   :: SDL.Renderer
  , wWidgetTree :: TMVar WidgetTree
  , idSet       :: TMVar (Set T.WidgetId)
  , updatable   :: Behavior Bool
  , withUpdatable :: IO () -> IO ()
  }

-- | Events of each Window
data WinEvents = WinEvents
  { weClosed :: Event ()
  , wePML :: Event (T.Point Int)
  , weRML :: Event (T.Point Int)
  , weMMPos :: Event (T.Point Int) -- Mouse motiong
  }

data WinConfig = WinConfig
  {
    winTitle :: T.Text
  , winResizable :: Bool
  , winInitialSize :: V2 Int
  }

newWindow :: WinConfig -> FrameEvents -> IO Window
newWindow cnf es = do
  sWin <- SDL.createWindow (winTitle cnf) winConf
  let es = mkEvents sWin
  (behUp, pushUp) <- sync $ newBehavior True
  let withUp = bracket_ (sync $ pushUp False) (sync $ pushUp True)
  win <- Window sWin es
          <$> SDL.createRenderer sWin (-1) SDL.defaultRenderer
          <*> atomically (newTMVar (Container DirV []))
          <*> atomically (newTMVar Set.empty)
          <*> pure behUp
          <*> pure withUp
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
      , weMMPos = snd <$> matchWindow fst (eMMPos es)
      }
      where
        matchWindow f = filterE ((==win) . f)

freeWindow :: Window -> IO ()
freeWindow w = do
  freeWT =<< (atomically . readTMVar . wWidgetTree $ w)
  SDL.destroyRenderer $ wRenderer w
  SDL.destroyWindow $ wWindow w
  where
    freeWT (Widget _ ms a) = do
      SDL.freeSurface =<< readMVar ms
      free a
    freeWT (Container _ ws) = mapM_ freeWT ws

withWindow :: WinConfig -> Tunagui -> (Window -> IO a) -> IO a
withWindow cnf t = bracket (newWindow cnf events) freeWindow
  where
    events = cntEvents t

generateWidId :: Window -> IO T.WidgetId
generateWidId win = atomically $ do
  (is, v) <- work <$> takeTMVar t
  putTMVar t is
  return v
  where
    t = idSet win
    work a = if Set.null a
                then (Set.singleton 0, 0)
                else let v = Set.findMax a + 1
                     in (Set.insert v a, v)

-- Layout **********************************************************************

data WidgetTree =
  forall a. (Show a, Renderable a)
  => Widget T.WidgetId (MVar SDL.Surface) a | Container Direction [WidgetTree]

newWidget :: (Show a, Renderable a) => Window -> a -> IO WidgetTree
newWidget win a =
  Widget <$> generateWidId win
         <*> (newMVar =<< SDL.createRGBSurface (V2 1 1) 32 (V4 0 0 0 0))
         <*> pure a

data Direction
  = DirH -- Horizontal
  | DirV -- Vertical
  deriving Show

instance Show WidgetTree where
  show (Widget i _ a) = "Widget#" ++ show i ++ " " ++ show a
  show (Container dir ws) = "Container " ++ show dir ++ " " ++ show ws

-- |
-- Fix the location of WidgetTree
locateWT :: Window -> Set T.WidgetId -> IO ()
locateWT w reshapeIds = do
  tree <- atomically . readTMVar . wWidgetTree $ w
  withUpdatable w $
    void $ go tree (T.P (V2 0 0)) False
  where
    go :: WidgetTree -> T.Point Int -> Bool -> IO (Bool, T.Range Int)
    go (Widget wid _ a) p0 pre = do
      let isUpdated = pre || Set.member wid reshapeIds
      when isUpdated $ locate a p0
      (,) isUpdated <$> range a
    go (Container dir ws) p0 pre = runStateT (locateList ws p0 pre) (T.R p0 p0)
      where
        locateList :: MonadIO m => [WidgetTree] -> T.Point Int -> Bool -> StateT (T.Range Int) m Bool
        locateList []     _ pre' = return pre'
        locateList (a:as) p pre' = do
          (b,r) <- liftIO $ go a p pre'
          modify (expand r)
          locateList as (nextPt r) b

        expand :: T.Range Int -> T.Range Int -> T.Range Int
        expand ra rb =
          T.R (T.P (V2 (ax0 `min` bx0) (ay0 `min` by0))) (T.P (V2 (ax1 `max` bx1) (ay1 `max` by1)))
          where
            (T.R (T.P (V2 ax0 ay0)) (T.P (V2 ax1 ay1))) = ra
            (T.R (T.P (V2 bx0 by0)) (T.P (V2 bx1 by1))) = rb

        nextPt :: T.Range Int -> T.Point Int
        nextPt (T.R (T.P (V2 x0 y0)) (T.P (V2 x1 y1))) =
          case dir of
            DirH -> T.P (V2 x1 y0)
            DirV -> T.P (V2 x0 y1)

-- |
-- Render all widgets in WidgetTree.
renderWT :: WidgetTree -> RenderP TunaguiT ()
renderWT (Widget _ _ a)   = render a
renderWT (Container _ ws) = mapM_ renderWT ws

mkUpdateEventWT :: Window -> IO (Event [(T.WidgetId, T.UpdateType)])
mkUpdateEventWT win = do
  t <- atomically . readTMVar . wWidgetTree $ win
  return $ go t
  where
    behUp = updatable win
    --
    go :: WidgetTree -> Event [(T.WidgetId, T.UpdateType)]
    go (Widget wid _ a) = (\t -> [(wid,t)]) <$> gate (update a) behUp
    go (Container _ ws) = foldl1' (mergeWith (++)) $ map go ws

-- *****************************************************************************

-- | One dimensional size
data DimSize a
  = Absolute a
  | RelContent
  deriving (Eq, Show)
