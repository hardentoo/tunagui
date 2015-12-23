{-# LANGUAGE GADTs #-}

-- Users can use these operations.

module Tunagui.Operation
  (
    TunaguiP, interpret
  --
  , testOperation
  , mkButton
  , onClick
  ) where

import Control.Monad.Operational
import Control.Monad.Reader (asks)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import FRP.Sodium

import qualified Tunagui.General.Types as T
import qualified Tunagui.General.Data as D
import Tunagui.Internal.Base
import qualified Tunagui.Internal.Operation.Render.SDL as R

import qualified Tunagui.Widgets.Prim.Button as Button

-- *****************************************************************************
data TunaguiI a where
  TestOperation :: TunaguiI ()
  -- make widgets
  MkButton      :: TunaguiI Button.Button
  -- widget operation
  OnClick       :: Button.Button -> TunaguiI (Event (T.Point Int)) -- Clickable a

type TunaguiP m a = ProgramT TunaguiI m a

interpret :: TunaguiP Base a -> Base a
interpret is = eval =<< viewT is

-- *****************************************************************************
testOperation = singleton TestOperation
mkButton      = singleton MkButton
onClick       = singleton . OnClick

-- *****************************************************************************
eval :: ProgramViewT TunaguiI Base a -> Base a
eval (Return a) = return a

eval (TestOperation :>>= is) = do
  -- test mouse button click
  e <- asks (D.ePML . D.cntEvents)
  liftIO . sync $ listen e print
  --
  r <- asks (D.twRenderer . D.cntTWindow)
  liftIO $ do
    print "in Tunagui Monad!"
    R.runRender r $ do
      R.setColor (V4 255 0 0 255)
      R.clear
      R.setColor (V4 255 255 255 255)
      R.drawRect (T.P (V2 100 100)) (T.S (V2 100 100))
      R.flush
  interpret (is ())

-- make widgets ================================================================
eval (MkButton :>>= is) = interpret . is =<< Button.newButton

-- widget operation ============================================================
eval (OnClick btn :>>= is) = interpret . is $ Button.onClickButton btn