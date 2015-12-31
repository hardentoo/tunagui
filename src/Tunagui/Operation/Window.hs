{-# LANGUAGE GADTs #-}

-- Users can use these operations.

module Tunagui.Operation.Window where

import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Operational
import           Control.Monad.Reader        (ask, asks)
import           Control.Concurrent          (forkIO)
import           FRP.Sodium
import           GHC.Conc.Sync
import           Linear.V2                   (V2 (..))
import           Linear.V4                   (V4 (..))
import Data.Text (Text)

import qualified Tunagui.General.Data        as D
import qualified Tunagui.General.Types       as T
import           Tunagui.General.Base        (TunaguiT, runTuna)
import           Tunagui.Internal.Render.SDL (runRender)
import qualified Tunagui.Internal.Render     as R

import           Tunagui.Widget.Component.Features
import           Tunagui.General.Layout

import qualified Tunagui.Widget.Prim.Button  as Button
import qualified Tunagui.Widget.Prim.Label   as Label

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
      liftIO . atomically $ writeTVar (D.wWidgetTree w) tree
      --
      tuna <- ask
      liftIO . sync $
        listen (updateEventWT tree) $ \_ -> do
          -- render tree
          putStrLn "update!"
          void . forkIO $ do
            locateWT tree
            runTuna tuna $ runRender (D.wRenderer w) $ render tree
      --
      interpret w (is ())
    eval w (TestRenderTree :>>= is) = do
      tree <- liftIO $ do
        tree <- atomically . readTVar $ D.wWidgetTree w
        locateWT tree
        return tree
      runRender (D.wRenderer w) $ render tree
      interpret w (is ())

    eval w (MkButton cfg :>>= is) = do
      ret <- genWT <$> Button.newButton cfg w
      interpret w (is ret)
    eval w (MkLabelT cfg text :>>= is) = do
      ret <- genWT <$> Label.newLabelT cfg w text
      interpret w (is ret)
    eval w (MkLabelB cfg beh :>>= is) = do
      ret <- genWT <$> Label.newLabelB cfg w beh
      interpret w (is ret)

    render tree = do
      R.setColor $ V4 240 240 240 255
      R.clear
      renderWT tree
      R.flush

-- *****************************************************************************
-- utilities
genWT :: (Show a, Renderable a) => a -> (a, WidgetTree)
genWT a = (a, Widget a)
