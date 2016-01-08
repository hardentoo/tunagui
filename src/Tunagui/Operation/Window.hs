{-# LANGUAGE GADTs #-}

-- Users can use these operations.

module Tunagui.Operation.Window where

import           Control.Monad               (void, unless)
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
import qualified Tunagui.Internal.Render     as R
import           Tunagui.Internal.Render     (runRender)

import           Tunagui.Widget.Component.Features

import qualified Tunagui.Widget.Prim.Button  as Button
-- import qualified Tunagui.Widget.Prim.Label   as Label

-- TODO: Function inserting WidgetTree to Window

data WindowI a where
  TestOverwriteTree :: WidgetTree -> WindowI ()
  TestRenderTree :: WindowI ()
  MkButton :: Button.Config -> WindowI (Button.Button, WidgetTree)
  -- MkLabelT :: Label.Config -> Text -> WindowI (Label.Label, WidgetTree)
  -- MkLabelB :: Label.Config -> Behavior Text -> WindowI (Label.Label, WidgetTree)

type WindowP m a = ProgramT WindowI m a

-- *****************************************************************************
testOverwriteTreeOP = singleton . TestOverwriteTree
testRenderTree = singleton TestRenderTree

newButton :: Button.Config -> ProgramT WindowI m (Button.Button, WidgetTree)
newButton = singleton . MkButton

-- newLabelT :: Label.Config -> Text -> ProgramT WindowI m (Label.Label, WidgetTree)
-- newLabelT c t = singleton $ MkLabelT c t
--
-- newLabelB :: Label.Config -> Behavior Text -> ProgramT WindowI m (Label.Label, WidgetTree)
-- newLabelB c b = singleton $ MkLabelB c b

-- *****************************************************************************
runWin :: D.Window -> WindowP TunaguiT a -> TunaguiT a
runWin = interpret
  where
    interpret :: D.Window -> WindowP TunaguiT a -> TunaguiT a
    interpret w is = eval w =<< viewT is

    eval :: D.Window -> ProgramViewT WindowI TunaguiT a -> TunaguiT a
    eval _ (Return a) = return a
    eval w (TestOverwriteTree tree :>>= is) = do
      liftIO $ print tree -- test
      tuna <- ask
      liftIO $ do
        _ <- atomically $ swapTMVar (D.wWidgetTree w) tree
        eUp <- mkUpdateEventWT w -- TODO: Listen it again when WidgetTree is updated
        sync $
          listen eUp $ \_ ->
            -- Render tree
            void . forkIO $ do
              putStrLn "update"
              locateWT w
              -- t <- atomically . readTMVar . D.wWidgetTree $ w
              -- runTuna tuna $ render' (D.wRenderer w) t

      interpret w (is ())
    eval w (TestRenderTree :>>= is) = do
      tree <- liftIO $ do
        locateWT w
        atomically . readTMVar . D.wWidgetTree $ w
      -- render' (D.wRenderer w) tree
      interpret w (is ())

    eval w (MkButton cfg :>>= is) =
      (interpret w . is) =<< genWT w =<< Button.mkButton cfg w
    -- eval w (MkLabelT cfg text :>>= is) = do
    --   (beh,_) <- liftIO . sync $ newBehavior text
    --   (interpret w . is) =<< genWT w =<< Label.mkLabel cfg w beh
    -- eval w (MkLabelB cfg beh :>>= is) =
    --   (interpret w . is) =<< genWT w =<< Label.mkLabel cfg w beh

    -- render' r tree = do
    --   runRender r $ do
    --     R.setColor $ V4 45 45 45 255
    --     R.clear
    --   renderWT r tree
    --   runRender r R.flush

-- *****************************************************************************
-- utilities
genWT :: (MonadIO m, Show a, Renderable a) => D.Window -> a -> m (a, WidgetTree)
genWT win a = liftIO $ (,) a <$> newWidget win a
