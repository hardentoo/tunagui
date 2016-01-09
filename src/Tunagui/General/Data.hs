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
  , mkUpdateEventWT
  , updateStateWT
  , DimSize (..)
  ) where

import           Control.Monad         (void, foldM, when)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Trans.State
import           Control.Monad.Reader (ask)
import           Data.List               (foldl', foldl1')
import           Control.Exception     (bracket, bracket_)
import qualified Data.Text             as T
import           FRP.Sodium
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TMVar
import           GHC.Conc.Sync (STM, atomically)
import           Linear.V2
import           Linear.V4
import qualified Linear.Affine as A
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Word (Word8)
import           Data.Tree (Tree (..))

import qualified SDL
import           SDL (($=))

import           Tunagui.General.Base  (Tunagui (..), FrameEvents (..), TunaguiT)
import qualified Tunagui.General.Types as T
import           Tunagui.Widget.Component.Features (Renderable, size, render, locate, update, resize, free)
import           Tunagui.Internal.Render

-- Window
data Window = Window
  { wWindow     :: SDL.Window
  , wEvents     :: WinEvents
  , wRenderer   :: MVar SDL.Renderer
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
          <$> (newMVar =<< SDL.createRenderer sWin (-1) SDL.defaultRenderer)
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
  => Widget T.WidgetId (TMVar Word8) (MVar SDL.Texture) a
   | Container Direction [WidgetTree]

newWidget :: (MonadIO m, Show a, Renderable a) => Window -> a -> m WidgetTree
newWidget win prim = liftIO $ do
  wid <- generateWidId win
  mTexture <- withMVar mr $ \r ->
    newMVar =<< SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget (V2 1 1)
  c <- atomically $ newTMVar 0
  --
  sync $ do
    listen (resize prim) $ \(T.S size) ->
      modifyMVar_ mTexture $ \texture -> do
        putStrLn $ "Recreate texture! " ++ show wid
        SDL.destroyTexture texture
        withMVar mr $ \r ->
          SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget (fromIntegral <$> size)
    --
    listen (update prim) $ \_ -> void . forkIO $ do
      putStrLn $ "Rendering itself! " ++ show wid
      withMVar mr $ \r -> -- (1) Lock Renderer
        withMVar mTexture $ \t -> -- (2) Lock Texture
          runRender r $
            onTexture t $
              render prim
      -- Notify updated
      putStrLn $ "Notify updated! " ++ show wid
      atomically $ do
        i <- takeTMVar c
        putTMVar c (i + 1)
      putStrLn $ "fin" ++ show wid

  return $ Widget wid c mTexture prim
  where
    mr = wRenderer win

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
    go wt@(Widget _ _ mTex a) tp@(T.P p) = do
      r <- ask
      liftIO $ do
        locate a tp
        (T.S sz) <- size a
        let rect = SDL.Rectangle (A.P (fromIntegral <$> p)) (fromIntegral <$> sz)
        withMVar mTex $ \tex -> SDL.copy r tex Nothing (Just rect)
    go wt@(Container _ as) (T.P p) = do
      r <- ask
      (ps, sz, rect) <- liftIO $ do
        (ps, T.S sz) <- sizeWT wt
        let sz' = fromIntegral <$> sz
            rect = SDL.Rectangle (A.P (fromIntegral <$> p)) sz'
        return (ps, sz, rect)
      withTexture sz $ \cntTex -> do
        onTexture cntTex$
          mapM_ (uncurry go) $ zip as ps
        copy cntTex Nothing (Just rect)

sizeWT :: MonadIO m => WidgetTree -> m ([T.Point Int], T.Size Int)
sizeWT (Widget _ _ _ a) = liftIO $ (,) [T.P (V2 0 0)] <$> size a
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

mkUpdateEventWT :: Window -> IO (Event ())
mkUpdateEventWT win = do
  t <- atomically . readTMVar . wWidgetTree $ win
  return $ go t
  where
    behCanUp = updatable win
    --
    go :: WidgetTree -> Event ()
    go (Widget _ _ _ a) = gate (update a) behCanUp
    go (Container _ ws) = foldl1' merge $ map go ws

updateStateWT :: WidgetTree -> STM (Tree Word8)
updateStateWT (Widget _ ti _ _) = Node <$> readTMVar ti <*> pure []
updateStateWT (Container _ ws) = Node 0 <$> mapM updateStateWT ws

-- *****************************************************************************

-- | One dimensional size
data DimSize a
  = Absolute a
  | RelContent
  deriving (Eq, Show)
