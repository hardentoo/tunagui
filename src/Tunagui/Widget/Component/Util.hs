module Tunagui.Widget.Component.Util
  ( upD, upS
  , mkSizeBehav
  ) where

import Control.Monad (void)
import FRP.Sodium

import Tunagui.General.Types (UpdateType (..))
import Tunagui.General.Data (DimSize (..))

upD :: Behavior a -> Event UpdateType
upD beh = const Redraw <$> updates beh

upS :: Behavior a -> Event UpdateType
upS beh = const Reshape <$> updates beh

mkSizeBehav :: (Ord a, Num a) =>
  DimSize a ->
  Maybe a -> -- minimum
  Maybe a -> -- maximum
  a -> -- padding
  a -> -- padding
  Behavior a ->
  Reactive (Behavior a)
mkSizeBehav dimA minA maxA padding1 padding2 behContent =
  fmap work <$> case dimA of
    Absolute a -> fst <$> newBehavior a
    RelContent -> return behContent
  where
    conv f (Just x) = f x
    conv _ Nothing  = id
    --
    padding = padding1 + padding2
    work = (+ padding) . conv max minA . conv min maxA
