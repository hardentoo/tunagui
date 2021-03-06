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
  , renderWT
  , sizeWT -- for debug
  -- , mkUpdateEventWT
  , updateStateWT
  , DimSize (..)
  ) where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TMVar
import           Control.Exception                 (bracket)
import           Control.Monad                     (void, when)
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad.Reader              (ask)
import           Control.Monad.Trans.State
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import           Data.Tree                         (Tree (..))
import           FRP.Sodium
import           GHC.Conc.Sync                     (STM, atomically)
import qualified Linear.Affine                     as A
import           Linear.V2
import           Linear.V4

import           SDL                               (($=))
import qualified SDL

import           Tunagui.General.Base              (FrameEvents (..),
                                                    Tunagui (..))
import qualified Tunagui.General.Types             as T
import           Tunagui.Internal.Render
import           Tunagui.Widget.Component.Features (Renderable, free, locate,
                                                    render, resized, sizeof,
                                                    updated)

-- Window
data Window = Window
  { wWindow     :: SDL.Window
  , wEvents     :: WinEvents
  , wRenderer   :: MVar SDL.Renderer
  , wWidgetTree :: TMVar WidgetTree
  , idSet       :: TMVar (Set T.WidgetId)
  }

-- | Events of each Window
data WinEvents = WinEvents
  { weClosed :: Event ()
  , wePML    :: Event (T.Point Int)
  , weRML    :: Event (T.Point Int)
  , weMMPos  :: Event (T.Point Int) -- Mouse motiong
  }

data WinConfig = WinConfig
  {
    winTitle       :: T.Text
  , winResizable   :: Bool
  , winInitialSize :: V2 Int
  }

newWindow :: WinConfig -> FrameEvents -> IO Window
newWindow cnf es = do
  putStrLn "* creating window"
  sWin <- SDL.createWindow (winTitle cnf) winConf
  let winEvents = mkEvents sWin
  win <- Window sWin winEvents
          <$> (newMVar =<< SDL.createRenderer sWin (-1) SDL.defaultRenderer)
          <*> atomically (newTMVar (Container DirV []))
          <*> atomically (newTMVar Set.empty)
  sync $ listen (weClosed winEvents) $ \_ -> freeWindow win -- TODO: Check if thread leak occurs
  putStrLn "* finished creating window"
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
  putStrLn "freeWindow"
  freeWT =<< (atomically . readTMVar . wWidgetTree $ w)
  SDL.destroyRenderer =<< readMVar (wRenderer w)
  SDL.destroyWindow $ wWindow w
  where
    freeWT (Widget _ _ mTex a) = do
      SDL.destroyTexture =<< readMVar mTex
      free a
    freeWT (Container _ ws) = mapM_ freeWT ws

withWindow :: WinConfig -> Tunagui -> (Window -> IO a) -> IO a
withWindow cnf t = bracket (newWindow cnf events) freeWindow
  where
    events = cntEvents t

withRenderer :: Window -> String -> (SDL.Renderer -> IO a) -> IO a
withRenderer win str f = do
  putStrLn $ "<Renderer: " ++ str ++ " start"
  ret <- withMVar mr f
  putStrLn $ ">Renderer: " ++ str ++ " end"
  return ret
  where
    mr = wRenderer win

generateWidId :: MonadIO m => Window -> m T.WidgetId
generateWidId win = liftIO . atomically $ do
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
  => Widget T.WidgetId (TMVar Int) (MVar SDL.Texture) a
   | Container Direction [WidgetTree]

newWidget :: (MonadIO m, Show a, Renderable a) => Window -> a -> m WidgetTree
newWidget win prim = liftIO $ do
  putStrLn "@newWidget start"
  wid <- generateWidId win
  tex <- withRenderer win "<newWidget>" $ \r -> runRender r $ createTexture (V2 1 1)
  mTexture <- newMVar tex
  cntr <- atomically $ newTMVar 1

  sync $ do
    listen (resized prim) $ newTexture wid mTexture
    listen (updated prim) $ \_ -> atomically $ do
      i <- takeTMVar cntr
      putTMVar cntr (i + 1)

  newTexture wid mTexture =<< sizeof prim

  putStrLn "@newWidget end"
  return $ Widget wid cntr mTexture prim
  where
    newTexture wid mTexture (T.S size) = liftIO $ do
      putStrLn " newTexture start"
      withRenderer win ("<newTexture>" ++ show wid) $ \r -> -- (1) Lock Renderer
        modifyMVar_ mTexture $ \texture -> do -- (2) Lock Texture
          SDL.destroyTexture texture
          runRender r $ createTexture size
      putStrLn " newTexture end"

data Direction
  = DirH -- Horizontal
  | DirV -- Vertical
  deriving Show

instance Show WidgetTree where
  show (Widget i _ _ a) = "Widget#" ++ show i ++ " " ++ show a
  show (Container dir ws) = "Container " ++ show dir ++ " " ++ show ws

-- |
-- Render all widgets in WidgetTree.
renderWT :: WidgetTree -> RenderT ()
renderWT tree = go tree (T.P (V2 0 0))
  where
    go :: WidgetTree -> T.Point Int -> RenderT ()
    go (Widget _ ti mTex prim) tp@(T.P p) = do
      r <- ask
      liftIO $ locate prim tp -- 'locate' isn't this function's work
      liftIO $ do -- Rendering if prim is updated
        i <- atomically . readTMVar $ ti
        when (i > 0) $ do
          withMVar mTex $ \tex ->
            runRender r $
              onTexture tex $ do
                setColor $ V4 255 255 0 0
                clear
                render prim
          void . atomically $ swapTMVar ti 0
      --
      (T.S sz) <- liftIO $ sizeof prim
      let rect = SDL.Rectangle (A.P (fromIntegral <$> p)) (fromIntegral <$> sz)
      tex <- liftIO $ readMVar mTex
      copy tex Nothing (Just rect)
    go wt@(Container _ as) (T.P p) = do
      (ps, sz, rect) <- liftIO $ do
        (ps, T.S sz) <- sizeWT wt
        let sz' = fromIntegral <$> sz
            rect = SDL.Rectangle (A.P (fromIntegral <$> p)) sz'
        return (ps, sz, rect)
      withTexture sz $ \cntTex -> do
        SDL.textureBlendMode cntTex $= SDL.BlendAlphaBlend
        onTexture cntTex $ do
          setColor $ V4 0 0 255 0
          clear
          mapM_ (uncurry go) $ zip as ps
        copy cntTex Nothing (Just rect)

sizeWT :: MonadIO m => WidgetTree -> m ([T.Point Int], T.Size Int)
sizeWT (Widget _ _ _ prim) = liftIO $ (,) [T.P (V2 0 0)] <$> sizeof prim
sizeWT (Container dir as) = do
  (ps, T.R _ (T.P s)) <- runStateT (go as p0) (T.R p0 p0)
  return (ps, T.S s)
  where
    p0 = T.P (V2 0 0)
    go :: MonadIO m => [WidgetTree] -> T.Point Int -> StateT (T.Range Int) m [T.Point Int]
    go []     _ = return []
    go (w:ws) p = do
      r <- liftIO $ toRange . snd <$> sizeWT w
      modify (expand r)
      ps <- go ws (nextPt r)
      return (p:ps)
      where
        toRange sz = T.R p (p `T.plusPS` sz)

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

updateStateWT :: WidgetTree -> STM (Tree (T.WidgetId, Int))
updateStateWT (Widget wid ti _ _) = do
  i <- readTMVar ti
  return $ Node (wid, i) []
updateStateWT (Container _ ws) = Node (0,0) <$> mapM updateStateWT ws

-- *****************************************************************************

-- | One dimensional size
data DimSize a
  = Absolute a
  | RelContent
  deriving (Eq, Show)
