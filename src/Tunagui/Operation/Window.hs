{-# LANGUAGE GADTs #-}

-- Users can use these operations.

module Tunagui.Operation.Window where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Operational
import           Control.Monad.Reader        (asks)
import           FRP.Sodium
import           GHC.Conc.Sync
import           Linear.V2                   (V2 (..))
import           Linear.V4                   (V4 (..))
import Data.Text (Text)

import qualified Tunagui.General.Data        as D
import qualified Tunagui.General.Types       as T
import           Tunagui.General.Base        (TunaguiT)
import           Tunagui.Internal.Render.SDL (runRender)
import qualified Tunagui.Internal.Render     as R

import           Tunagui.Widget.Features
import           Tunagui.General.Layout

import qualified Tunagui.Widget.Prim.Button  as Button
import qualified Tunagui.Widget.Prim.Label   as Label

data WindowI a where
  TestOverwriteTree :: WidgetTree -> WindowI ()
  TestRenderTree :: WindowI ()
  MkButton :: Button.ButtonConfig -> WindowI (Button.Button, WidgetTree)
  MkLabel :: Label.LabelConfig -> Text -> WindowI (Label.Label, WidgetTree)

type WindowP m a = ProgramT WindowI m a

-- *****************************************************************************
testOverwriteTreeOP = singleton . TestOverwriteTree
testRenderTree = singleton TestRenderTree

mkButton :: Button.ButtonConfig -> ProgramT WindowI m (Button.Button, WidgetTree)
mkButton = singleton . MkButton

mkLabel :: Label.LabelConfig -> Text -> ProgramT WindowI m (Label.Label, WidgetTree)
mkLabel c t = singleton $ MkLabel c t

-- *****************************************************************************
runTWin :: D.Window -> WindowP TunaguiT a -> TunaguiT a
runTWin = interpret
  where
    interpret :: D.Window -> WindowP TunaguiT a -> TunaguiT a
    interpret w is = eval w =<< viewT is

    eval :: D.Window -> ProgramViewT WindowI TunaguiT a -> TunaguiT a
    eval _  (Return a) = return a
    eval w (TestOverwriteTree tree :>>= is) = do
      liftIO . atomically $ writeTVar (D.wWidgetTree w) tree
      interpret w (is ())
    eval w (TestRenderTree :>>= is) = do
      tree <- liftIO $ do
        tree <- atomically . readTVar $ D.wWidgetTree w
        locateWT tree
        return tree
      runRender (D.wRenderer w) $ do
        R.setColor $ V4 240 240 240 255
        R.clear
        renderWT tree
        R.flush
      interpret w (is ())

    eval w (MkButton cfg :>>= is) = do
      ret <- genWT <$> Button.newButton cfg w
      interpret w (is ret)
    eval w (MkLabel cfg text :>>= is) = do
      ret <- genWT <$> Label.newLabelT cfg w text
      interpret w (is ret)

-- *****************************************************************************
-- utilities
genWT :: (Show a, Renderable a) => a -> (a, WidgetTree)
genWT a = (a, Widget a)
