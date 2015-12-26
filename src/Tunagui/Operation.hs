{-# LANGUAGE GADTs #-}

-- Users can use these operations.

module Tunagui.Operation where

import           Control.Monad.IO.Class                (MonadIO)
import           Control.Monad.Operational
import           Control.Monad.Reader                  (asks)
import           FRP.Sodium
import           GHC.Conc.Sync
import           Linear.V2                             (V2 (..))
import           Linear.V4                             (V4 (..))

import qualified Tunagui.General.Data                  as D
import qualified Tunagui.General.Types                 as T
import           Tunagui.Internal.Base
import qualified Tunagui.Internal.Operation.Render.SDL as R

import           Tunagui.Widget.Features
import           Tunagui.Widget.Layout
import qualified Tunagui.Widget.Prim.Button            as Button

data TWindowI a where
  TestOverwriteTree :: WidgetTree -> TWindowI ()
  TestRenderTree :: TWindowI ()
  MkButton :: Button.ButtonConfig -> TWindowI (Button.Button, WidgetTree)

type TWindowP m a = ProgramT TWindowI m a

-- *****************************************************************************
testOverwriteTreeOP = singleton . TestOverwriteTree
testRenderTree = singleton TestRenderTree

mkButton :: Button.ButtonConfig -> ProgramT TWindowI m (Button.Button, WidgetTree)
mkButton = singleton . MkButton

-- *****************************************************************************
runTWin :: MonadIO m => D.TWindow -> TWindowP m a -> m a
runTWin = interpret
  where
    interpret :: MonadIO m => D.TWindow -> TWindowP m a -> m a
    interpret tw is = eval tw =<< viewT is

    eval :: MonadIO m => D.TWindow -> ProgramViewT TWindowI m a -> m a
    eval _  (Return a) = return a
    eval twin (TestOverwriteTree tree :>>= is) = do
      liftIO $ D.testOverwriteTree tree twin
      interpret twin (is ())
    eval twin (TestRenderTree :>>= is) = do
      liftIO $ do
        tree <- atomically . readTVar $ D.twWidgetTree twin
        locateWT tree
        R.runRender (D.twRenderer twin) $ do
          R.setColor $ V4 240 240 240 255
          R.clear
          renderWT tree
          R.flush
      interpret twin (is ())

    eval twin (MkButton cfg :>>= is) = do
      r <- genWT <$> liftIO (Button.newButton cfg (D.twEvents twin))
      interpret twin (is r)

-- *****************************************************************************
-- utilities
genWT :: (Show a, Renderable a) => a -> (a, WidgetTree)
genWT a = (a, Widget a)
