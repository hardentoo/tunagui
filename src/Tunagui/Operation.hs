{-# LANGUAGE GADTs #-}

-- Users can use these operations.

module Tunagui.Operation where

import Control.Monad.Operational
import Control.Monad.Reader (asks)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import FRP.Sodium

import qualified Tunagui.General.Types as T
import qualified Tunagui.General.Data as D
import Tunagui.Internal.Base
import qualified Tunagui.Internal.Operation.Draw.SDL as R

data TunaguiI a where
  TestOperation :: TunaguiI ()

type TunaguiP m a = ProgramT TunaguiI m a

testOperation :: (Monad m) => TunaguiP m ()
testOperation = singleton TestOperation

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

interpret :: TunaguiP Base a -> Base a
interpret is = eval =<< viewT is

eval :: ProgramViewT TunaguiI Base a -> Base a
eval (Return a) = return a

eval (TestOperation :>>= is) = do
  -- test mouse button click
  e <- asks (D.ePML . D.cntEvents)
  liftIO . sync $ listen e print
  --
  tw <- asks D.cntTWindow
  liftIO $ do
    print "in Tunagui Monad!"
    R.runDraw tw $ do
      R.setColor (V4 255 0 0 255)
      R.clear
      R.setColor (V4 255 255 255 255)
      R.drawRect (T.P (V2 100 100)) (T.S (V2 100 100))
      R.flush
  interpret (is ())
