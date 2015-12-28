{-# LANGUAGE GADTs #-}

-- Users can use these operations.

module Tunagui.Operation.TWindow where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Operational
import           Control.Monad.Reader        (asks)
import           FRP.Sodium
import           GHC.Conc.Sync
import           Linear.V2                   (V2 (..))
import           Linear.V4                   (V4 (..))

import qualified Tunagui.General.Data        as D
import qualified Tunagui.General.Types       as T
import           Tunagui.General.Base        (TunaguiT)
import           Tunagui.Internal.Render.SDL (runRender)
import qualified Tunagui.Internal.Render     as R

import           Tunagui.Widget.Features
import           Tunagui.Widget.Layout
import qualified Tunagui.Widget.Prim.Button  as Button

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
runTWin :: D.TWindow -> TWindowP TunaguiT a -> TunaguiT a
runTWin = interpret
  where
    interpret :: D.TWindow -> TWindowP TunaguiT a -> TunaguiT a
    interpret tw is = eval tw =<< viewT is

    eval :: D.TWindow -> ProgramViewT TWindowI TunaguiT a -> TunaguiT a
    eval _  (Return a) = return a
    eval twin (TestOverwriteTree tree :>>= is) = do
      liftIO . atomically $ writeTVar (D.twWidgetTree twin) tree
      interpret twin (is ())
    eval twin (TestRenderTree :>>= is) = do
      tree <- liftIO $ do
        tree <- atomically . readTVar $ D.twWidgetTree twin
        locateWT tree
        return tree
      runRender (D.twRenderer twin) $ do
        R.setColor $ V4 240 240 240 255
        R.clear
        renderWT tree
        R.flush
      interpret twin (is ())

    eval twin (MkButton cfg :>>= is) = do
      ret <- genWT <$> Button.newButton cfg twin
      interpret twin (is ret)

-- *****************************************************************************
-- utilities
genWT :: (Show a, Renderable a) => a -> (a, WidgetTree)
genWT a = (a, Widget a)
