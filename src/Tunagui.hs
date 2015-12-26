module Tunagui
( Settings(..)
, withTunagui
, withTWindow
-- test
, testRenderTree
) where

import           Tunagui.General.Data       (Settings (..), withTWindow)
import           Tunagui.General.Initialize (withTunagui)

-----
-- TODO: Move below
import GHC.Conc.Sync (atomically, readTVar)

import qualified Tunagui.General.Data as D
import qualified Tunagui.Internal.Operation.Render.SDL as R
import Tunagui.Widget.Layout (locateWT, renderWT)
import Linear.V4

testRenderTree :: D.TWindow -> IO ()
testRenderTree tw = do
  tree <- atomically . readTVar $ tWTree
  locateWT tree
  R.runRender renderer $ do
    R.setColor $ V4 240 240 240 255
    R.clear
    renderWT tree
    R.flush
  where
    tWTree = D.twWidgetTree tw
    renderer = D.twRenderer tw
