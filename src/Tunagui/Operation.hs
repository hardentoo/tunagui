{-# LANGUAGE GADTs #-}

-- Users can use these operations.

module Tunagui.Operation
  (
    TunaguiP, interpret
  --
  , testOperation
  , testRenderTree
  , pushWidget
  , mkButton
  ) where

import           Control.Monad.Operational
import           Control.Monad.Reader                  (asks)
import           FRP.Sodium
import           GHC.Conc.Sync
import           Linear.V2                             (V2 (..))
import           Linear.V4                             (V4 (..))

import           Control.Arrow

import qualified Tunagui.General.Data                  as D
import qualified Tunagui.General.Types                 as T
import           Tunagui.Internal.Base
import qualified Tunagui.Internal.Operation.Render.SDL as R

import           Tunagui.Widget.Features
import           Tunagui.Widget.Layout
import qualified Tunagui.Widget.Prim.Button            as Button

-- *****************************************************************************
data TunaguiI a where
  TestOperation  :: TunaguiI ()
  TestRenderTree :: TunaguiI ()
  PushWidget     :: WidgetTree -> TunaguiI ()
  -- make widgets
  MkButton       :: Button.ButtonConfig -> TunaguiI (Button.Button, WidgetTree)

type TunaguiP m a = ProgramT TunaguiI m a

interpret :: TunaguiP Base a -> Base a
interpret is = eval =<< viewT is

-- *****************************************************************************
testOperation  = singleton TestOperation
testRenderTree = singleton TestRenderTree
pushWidget w   = singleton . PushWidget $ w
mkButton       = singleton . MkButton

-- *****************************************************************************
eval :: ProgramViewT TunaguiI Base a -> Base a
eval (Return a) = return a

-- Test for rendering
eval (TestOperation :>>= is) = do
  -- e <- asks (D.ePML . D.cntEvents)
  -- liftIO . sync $ listen e print
  --
  r <- asks (D.twRenderer . D.cntTWindow)
  liftIO . R.runRender r $ do
    R.setColor (V4 255 0 0 255)
    R.clear
    R.setColor (V4 255 255 255 255)
    R.drawRect (T.P (V2 100 100)) (T.S (V2 100 100))
    R.flush
  interpret (is ())

eval (TestRenderTree :>>= is) = do
  twin <- asks D.cntTWindow
  liftIO $ do
    tree <- atomically . readTVar . D.twWidgetTree $ twin
    locateWT tree
    R.runRender (D.twRenderer twin) $ do
      R.setColor $ V4 240 240 240 255
      R.clear
      renderWT tree
      R.flush
  interpret (is ())

eval (PushWidget w :>>= is) = do
  tTree <- asks (D.twWidgetTree . D.cntTWindow)
  liftIO . atomically $
    writeTVar tTree =<< pushW w <$> readTVar tTree
  interpret (is ())

-- make widgets ================================================================

-- | Make new Button.
eval (MkButton cfg :>>= is) =
  (interpret . is) =<< genWT =<< Button.newButton cfg

-- *****************************************************************************
-- Utilities

genWT :: (Show a, Renderable a) => a -> Base (a, WidgetTree)
genWT a = return (a, Widget a)
