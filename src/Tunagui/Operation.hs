{-# LANGUAGE GADTs #-}

-- Users can use these operations.

module Tunagui.Operation
  (
    TunaguiP, interpret
  --
  , testOperation
  , testRenderTree
  , quitBehav
  -- , pushWidget
  , mkButton
  ) where

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

-- *****************************************************************************
data TunaguiI a where
  TestOperation  :: D.TWindow -> TunaguiI ()
  TestRenderTree :: D.TWindow -> TunaguiI ()
  QuitBehav      :: TunaguiI (Behavior Bool)
  -- PushWidget     :: WidgetTree -> TunaguiI ()
  -- make widgets
  MkButton       :: Button.ButtonConfig -> TunaguiI (Button.Button, WidgetTree)

type TunaguiP m a = ProgramT TunaguiI m a

interpret :: TunaguiP Base a -> Base a
interpret is = eval =<< viewT is

-- *****************************************************************************
testOperation :: D.TWindow -> ProgramT TunaguiI m ()
testOperation = singleton . TestOperation
testRenderTree :: D.TWindow -> ProgramT TunaguiI m ()
testRenderTree = singleton . TestRenderTree
quitBehav :: ProgramT TunaguiI m (Behavior Bool)
quitBehav = singleton QuitBehav
-- pushWidget :: WidgetTree -> ProgramT TunaguiI m ()
-- pushWidget w = singleton . PushWidget $ w
mkButton :: Button.ButtonConfig -> ProgramT TunaguiI m (Button.Button, WidgetTree)
mkButton = singleton . MkButton

-- *****************************************************************************
eval :: ProgramViewT TunaguiI Base a -> Base a
eval (Return a) = return a

-- Test for rendering
eval (TestOperation tw :>>= is) = do
  -- e <- asks (D.ePML . D.cntEvents)
  -- liftIO . sync $ listen e print
  --
  liftIO . R.runRender (D.twRenderer tw) $ do
    R.setColor (V4 255 0 0 255)
    R.clear
    R.setColor (V4 255 255 255 255)
    R.drawRect (T.P (V2 100 100)) (T.S (V2 100 100))
    R.flush
  interpret (is ())

eval (TestRenderTree tw :>>= is) = do
  liftIO $ do
    tree <- atomically . readTVar . D.twWidgetTree $ tw
    locateWT tree
    R.runRender (D.twRenderer tw) $ do
      R.setColor $ V4 240 240 240 255
      R.clear
      renderWT tree
      R.flush
  interpret (is ())

eval (QuitBehav :>>= is) =
  interpret . is =<< asks (D.behQuit . D.cntEvents)

-- eval (PushWidget w :>>= is) = do
--   tTree <- asks (D.twWidgetTree . D.cntTWindow)
--   liftIO . atomically $
--     writeTVar tTree =<< pushW w <$> readTVar tTree
--   interpret (is ())

-- make widgets ================================================================

-- | Make new Button.
eval (MkButton cfg :>>= is) =
  (interpret . is) =<< genWT =<< Button.newButton cfg

-- *****************************************************************************
-- Utilities

genWT :: (Show a, Renderable a) => a -> Base (a, WidgetTree)
genWT a = return (a, Widget a)
