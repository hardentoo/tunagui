{-# LANGUAGE GADTs #-}
module Tunagui.Internal.Operation.Draw where

import           Data.Word                 (Word8)
import           Linear                    (V4 (..))

import           Control.Monad.Operational

import           Tunagui.General.Types (IPoint, ISize)

type Draw s = [s]

data DrawI a where
  Clear :: DrawI ()
  Flush :: DrawI ()
  SetColor :: V4 Word8 -> DrawI ()
  -- Figure
  FillRect :: IPoint -> ISize -> DrawI ()
  DrawRect :: IPoint -> ISize -> DrawI ()

type DrawP m a = ProgramT DrawI m a

clear :: DrawP m ()
clear = singleton Clear

flush :: DrawP m ()
flush = singleton Flush

setColor :: V4 Word8 -> DrawP m ()
setColor = singleton . SetColor

fillRect :: IPoint -> ISize -> DrawP m ()
fillRect p s = singleton $ FillRect p s

drawRect :: IPoint -> ISize -> DrawP m ()
drawRect p s = singleton $ DrawRect p s
