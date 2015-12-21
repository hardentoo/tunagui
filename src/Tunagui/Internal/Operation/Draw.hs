{-# LANGUAGE GADTs #-}
module Tunagui.Internal.Operation.Draw where

import           Data.Word                 (Word8)
import           Linear                    (V4 (..))

import           Control.Monad.Operational

type Draw s = [s]

data DrawI a where
  SetColor :: V4 Word8 -> DrawI ()

type DrawP m a = ProgramT DrawI m a

setColor :: (Monad m) => V4 Word8 -> DrawP m ()
setColor = singleton . SetColor
