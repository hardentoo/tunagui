{-# LANGUAGE GADTs #-}

-- Users can use these operations.

module Tunagui.Operation where

import Control.Monad.Operational
import Control.Monad.Reader (asks)
import Linear.V4 (V4(..))

import Tunagui.General.Data (cntTWindow)
import Tunagui.Internal.Base
import qualified Tunagui.Internal.Operation.Draw.SDL as Draw

data TunaguiI a where
  TestOperate :: TunaguiI ()

type TunaguiP m a = ProgramT TunaguiI m a

testOperate :: (Monad m) => TunaguiP m ()
testOperate = singleton TestOperate

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

interpret :: TunaguiP Base a -> Base a
interpret is = eval =<< viewT is

eval :: ProgramViewT TunaguiI Base a -> Base a
eval (Return a) = return a

eval (TestOperate :>>= is) = do
  tw <- asks cntTWindow
  liftIO $ do
    print "in Tunagui Monad!"
    Draw.interpret tw $ Draw.setColor (V4 255 0 0 255)
  interpret (is ())
