{-# LANGUAGE GADTs #-}
module Tunagui.Internal.Operation.Render where

import           Data.Word                 (Word8)
import           Linear                    (V4 (..))

import           Control.Monad.Operational

import           Tunagui.General.Types     (IPoint, ISize)

data RenderI a where
  Clear :: RenderI ()
  Flush :: RenderI ()
  SetColor :: V4 Word8 -> RenderI ()
  -- Figure
  FillRect :: IPoint -> ISize -> RenderI ()
  DrawRect :: IPoint -> ISize -> RenderI ()

type RenderP m a = ProgramT RenderI m a

clear :: RenderP m ()
clear = singleton Clear

flush :: RenderP m ()
flush = singleton Flush

setColor :: V4 Word8 -> RenderP m ()
setColor = singleton . SetColor

fillRect :: IPoint -> ISize -> RenderP m ()
fillRect p s = singleton $ FillRect p s

drawRect :: IPoint -> ISize -> RenderP m ()
drawRect p s = singleton $ DrawRect p s
