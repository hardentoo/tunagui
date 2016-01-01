{-# LANGUAGE ExistentialQuantification #-}

module Tunagui.General.Data
  (
    Window (..)
  , WinEvents (..)
  , WinConfig (..)
  , withWindow, newWindow, freeWindow
  , generateId
  -- *Layout
  , WidgetTree (..)
  , Direction (..)
  , newWidget
  , locateWT
  , renderWT
  , updateEventWT
  , DimSize (..)
  ) where

import           Control.Monad         (void, foldM)
import           Control.Monad.IO.Class  (MonadIO)
import           Data.List               (foldl', foldl1')
import           Control.Exception     (bracket)
import qualified Data.Text             as T
import           FRP.Sodium
import           Control.Concurrent.MVar (MVar, newMVar, modifyMVarMasked)
import           Linear.V2
import qualified SDL
import           Data.Set              (Set)
import qualified Data.Set              as Set

import           Tunagui.General.Base  (Tunagui (..), FrameEvents (..), TunaguiT)
import qualified Tunagui.General.Types as T
import           Tunagui.Widget.Component.Features (Renderable, locate, render, update)
import           Tunagui.Internal.Render (RenderP)

-- Window
data Window = Window
  { wWindow     :: SDL.Window
  , wEvents     :: WinEvents
  , wRenderer   :: SDL.Renderer
  , wWidgetTree :: MVar WidgetTree
  , idSet       :: MVar (Set T.WidgetId)
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
          <*> newMVar (Container DirV [])
          <*> newMVar Set.empty -- TODO: outside atomicatty
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

generateId :: Window -> IO T.WidgetId
generateId win = modifyMVarMasked (idSet win) work
  where
    work a = return $
      if Set.null a
        then (Set.singleton 0, 0)
        else let v = Set.findMax a + 1
             in (Set.insert v a, v)

-- Layout **********************************************************************

data WidgetTree =
  forall a. (Show a, Renderable a)
  => Widget T.WidgetId a | Container Direction [WidgetTree]

newWidget :: (Show a, Renderable a) => Window -> a -> IO WidgetTree
newWidget win a = Widget <$> generateId win <*> pure a

data Direction
  = DirH -- Horizontal
  | DirV -- Vertical
  deriving Show

instance Show WidgetTree where
  show (Widget i a) = "Widget#" ++ show i ++ " " ++ show a
  show (Container dir ws) = "Container " ++ show dir ++ " " ++ show ws

-- |
-- Fix the location of WidgetTree
locateWT :: WidgetTree -> IO ()
locateWT widgetTree = void . sync $ go widgetTree (T.P (V2 0 0))
  where
    go :: WidgetTree -> T.Point Int -> Reactive (T.Range Int)
    go (Widget _ a)         p0 = locate a p0
    go (Container dir ws) p0 = do
      ranges <- foldM locate' [T.R p0 p0] ws
      return $ T.R (foldl' leftTop p0 ranges) (foldl' rightBottom p0 ranges)
      where
        locate' :: [T.Range Int] -> WidgetTree -> Reactive [T.Range Int]
        locate' []       _    = undefined
        locate' rs@(r:_) tree = (:rs) <$> work tree (nextFrom r)
          where
            work (Widget _ a)        = locate a
            work cnt@(Container _ _) = go cnt

        leftTop :: Ord a => T.Point a -> T.Range a -> T.Point a
        leftTop point range =
          let (T.P (V2 x y)) = point
              (T.R (T.P (V2 x' y')) _) = range
          in T.P (V2 (min x x') (min y y'))

        rightBottom :: Ord a => T.Point a -> T.Range a -> T.Point a
        rightBottom point range =
          let (T.P (V2 x y)) = point
              (T.R _ (T.P (V2 x' y'))) = range
          in T.P (V2 (max x x') (max y y'))

        nextFrom (T.R (T.P (V2 x0 y0)) (T.P (V2 x1 y1))) =
          case dir of
            DirH -> T.P (V2 x1 y0)
            DirV -> T.P (V2 x0 y1)

-- |
-- Render all widgets in WidgetTree.
renderWT :: WidgetTree -> RenderP TunaguiT ()
renderWT (Widget _ a)       = render a
renderWT (Container _ ws) = mapM_ renderWT ws

updateEventWT :: WidgetTree -> Event [(T.WidgetId, String)]
updateEventWT (Widget wid a)   = (\t -> [(wid,t)]) <$> update a
updateEventWT (Container _ ws) = foldl1' (mergeWith (++)) $ map updateEventWT ws

-- *****************************************************************************

-- | One dimensional size
data DimSize a
  = Absolute a
  | RelContent
  deriving (Eq, Show)
