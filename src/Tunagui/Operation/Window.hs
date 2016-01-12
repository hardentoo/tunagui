{-# LANGUAGE GADTs #-}

-- Users can use these operations.

module Tunagui.Operation.Window where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Operational
import           Control.Monad.Reader        (ask)
import           GHC.Conc.Sync
import           Control.Concurrent.STM.TMVar
import           Linear.V4                   (V4 (..))
import           Data.Text (Text)
import           Data.Tree                   (flatten)

import qualified Tunagui.General.Data        as D
import           Tunagui.General.Data        (WidgetTree, renderWT, newWidget)
import           Tunagui.General.Base        (TunaguiT)
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
testOverwriteTreeOP :: WidgetTree -> ProgramT WindowI m ()
testOverwriteTreeOP = singleton . TestOverwriteTree

testRenderTree :: ProgramT WindowI m ()
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
      liftIO $ do
        _ <- atomically $ swapTMVar (D.wWidgetTree w) tree
        -- TODO: Set when Window is initialized
        let loop stWT = do -- TODO: Kill when this Window is destroyed
              putStrLn "!Render WidgetTree"
              print $ flatten stWT
              render' w
              next <- atomically $ do
                t <- readTMVar . D.wWidgetTree $ w
                newStWT <- D.updateStateWT t
                if newStWT == stWT
                  then retry
                  else return newStWT
              loop next
        -- Start with initial state
        curStWT <- atomically $ do
          t <- readTMVar . D.wWidgetTree $ w
          D.updateStateWT t
        forkIO $ loop curStWT
        return ()

      interpret w (is ())
    eval w (TestRenderTree :>>= is) =
      -- tree <- liftIO $ do
      --   locateWT w
      --   atomically . readTMVar . D.wWidgetTree $ w
      -- render' (D.wRenderer w) tree
      interpret w (is ())

    eval w (MkButton cfg :>>= is) =
      (interpret w . is) =<< genWT w =<< Button.mkButton cfg w
    -- eval w (MkLabelT cfg text :>>= is) = do
    --   (beh,_) <- liftIO . sync $ newBehavior text
    --   (interpret w . is) =<< genWT w =<< Label.mkLabel cfg w beh
    -- eval w (MkLabelB cfg beh :>>= is) =
    --   (interpret w . is) =<< genWT w =<< Label.mkLabel cfg w beh

    render' win = do
      tree <- atomically . readTMVar . D.wWidgetTree $ win
      withMVar mr $ \r ->
        runRender r $ do
          R.setColor $ V4 45 45 45 255
          R.clear
          renderWT tree
          R.flush
      where
        mr = D.wRenderer win

-- *****************************************************************************
-- utilities
genWT :: (MonadIO m, Show a, Renderable a) => D.Window -> a -> m (a, WidgetTree)
genWT win a = liftIO $ (,) a <$> newWidget win a
