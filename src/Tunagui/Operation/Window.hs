{-# LANGUAGE GADTs #-}

-- Users can use these operations.

module Tunagui.Operation.Window where

import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.RWS.Lazy (RWST, runRWST)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Operational
import           Control.Monad.Reader        (ask, asks)
import           Control.Concurrent          (forkIO)
import           FRP.Sodium
import           Control.Concurrent.MVar     (MVar, swapMVar, readMVar, withMVar)
import           GHC.Conc.Sync
import           Control.Concurrent.STM.TMVar
import           Linear.V2                   (V2 (..))
import           Linear.V4                   (V4 (..))
import           Data.Text (Text)
import qualified Data.Set as Set
import           Data.Set (Set)

import qualified Tunagui.General.Data        as D
import           Tunagui.General.Data        (WidgetTree, mkUpdateEventWT, locateWT, renderWT, newWidget)
import qualified Tunagui.General.Types       as T
import           Tunagui.General.Base        (Tunagui, TunaguiT, runTuna)
import           Tunagui.Internal.Render.SDL (runRender)
import qualified Tunagui.Internal.Render     as R

import           Tunagui.Widget.Component.Features

import qualified Tunagui.Widget.Prim.Button  as Button
import qualified Tunagui.Widget.Prim.Label   as Label

-- TODO: Function inserting WidgetTree to Window

data WindowI a where
  TestOverwriteTree :: WidgetTree -> WindowI ()
  TestRenderTree :: WindowI ()
  MkButton :: Button.Config -> WindowI (Button.Button, WidgetTree)
  MkLabelT :: Label.Config -> Text -> WindowI (Label.Label, WidgetTree)
  MkLabelB :: Label.Config -> Behavior Text -> WindowI (Label.Label, WidgetTree)

type WindowP m a = ProgramT WindowI m a

-- *****************************************************************************
testOverwriteTreeOP = singleton . TestOverwriteTree
testRenderTree = singleton TestRenderTree

mkButton :: Button.Config -> ProgramT WindowI m (Button.Button, WidgetTree)
mkButton = singleton . MkButton

mkLabelT :: Label.Config -> Text -> ProgramT WindowI m (Label.Label, WidgetTree)
mkLabelT c t = singleton $ MkLabelT c t

mkLabelB :: Label.Config -> Behavior Text -> ProgramT WindowI m (Label.Label, WidgetTree)
mkLabelB c b = singleton $ MkLabelB c b

-- *****************************************************************************
runWin :: D.Window -> WindowP TunaguiT a -> TunaguiT a
runWin = interpret
  where
    interpret :: D.Window -> WindowP TunaguiT a -> TunaguiT a
    interpret w is = eval w =<< viewT is

    eval :: D.Window -> ProgramViewT WindowI TunaguiT a -> TunaguiT a
    eval _  (Return a) = return a
    eval w (TestOverwriteTree tree :>>= is) = do
      liftIO $ print tree -- test
      tuna <- ask
      liftIO $ do
        _ <- atomically $ swapTMVar (D.wWidgetTree w) tree
        eUpdate <- mkUpdateEventWT w -- TODO: Listen it again when WidgetTree is updated
        sync $
          listen eUpdate $ \idUptypes -> do
            -- Render tree
            putStrLn $ "update: " ++ show idUptypes
            let reshapeIds = Set.fromList $ map fst $ filter ((== T.Reshape) . snd) idUptypes
                redrawIds = Set.fromList $ map fst $ filter ((== T.Redraw) . snd) idUptypes
            void . forkIO $ do
              locateWT w reshapeIds
              t <- atomically $ readTMVar $ D.wWidgetTree w
              runTuna tuna $ runRender (D.wRenderer w) $ render t

      interpret w (is ())
    eval w (TestRenderTree :>>= is) = do
      tree <- liftIO $ do
        locateWT w =<< (atomically . readTMVar . D.idSet $ w)
        atomically . readTMVar . D.wWidgetTree $ w
      runRender (D.wRenderer w) $ render tree
      interpret w (is ())

    eval w (MkButton cfg :>>= is) =
      (interpret w . is) =<< (liftIO . atomically . genWT w) =<< Button.newButton cfg w
    eval w (MkLabelT cfg text :>>= is) =
      (interpret w . is) =<< (liftIO . atomically . genWT w) =<< Label.newLabelT cfg w text
    eval w (MkLabelB cfg beh :>>= is) =
      (interpret w . is) =<< (liftIO . atomically . genWT w) =<< Label.newLabelB cfg w beh

    render tree = do
      R.setColor $ V4 240 240 240 255
      R.clear
      renderWT tree
      R.flush

-- *****************************************************************************
-- utilities
genWT :: (Show a, Renderable a) => D.Window -> a -> STM (a, WidgetTree)
genWT win a = (,) a <$> newWidget win a

-- renderWindow :: Tunagui -> D.Window -> [(T.WidgetId, T.UpdateType)] -> IO ()
-- renderWindow tuna win as = do
--   (_,redrawIds',_) <- runRWST (locateWT win) reshapeIds redrawIds
--   return ()
--   where
--     reshapeIds = Set.fromList $ map fst $ filter ((== T.Reshape) . snd) as
--     redrawIds = Set.fromList $ map fst $ filter ((== T.Redraw) . snd) as
