{-# LANGUAGE GADTs #-}

module Tunagui.Internal.Operation.Draw.SDL
  (
  -- Operation
    setColor
  -- SDL
  , interpret
  ) where

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Operational
import           Linear                          (V4 (..))

import           SDL                             (($=))
import qualified SDL

import           Tunagui.General.Data            (TWindow (..))
import           Tunagui.Internal.Operation.Draw

interpret :: TWindow -> DrawP IO a -> IO ()
interpret t is = eval t =<< viewT is

eval :: TWindow -> ProgramViewT DrawI IO a -> IO ()
eval _ (Return _) = return ()
eval t (SetColor c :>>= is) = do
  SDL.rendererDrawColor r $= c
  interpret t (is ())
  where
    r = twRenderer t
