{-# LANGUAGE GADTs #-}

-- Users can use these operations.

module Tunagui.Operation where

import Control.Monad.Operational
import Control.Monad.Reader (asks)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))

import qualified Tunagui.General.Types as T
import Tunagui.General.Data (cntTWindow)
import Tunagui.Internal.Base
import qualified Tunagui.Internal.Operation.Draw.SDL as D

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
  tw <- asks cntTWindow
  liftIO $ do
    print "in Tunagui Monad!"
    D.runDraw tw $ do
      D.setColor (V4 255 0 0 255)
      D.clear
      D.setColor (V4 255 255 255 255)
      D.drawRect (T.P (V2 100 100)) (T.S (V2 100 100))
      D.flush
  interpret (is ())
