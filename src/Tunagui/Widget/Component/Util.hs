module Tunagui.Widget.Component.Util
  ( upD, upS
  , mkDimBehav
  ) where

import Control.Monad (void)
import FRP.Sodium

import Tunagui.General.Types (UpdateType (..))
import Tunagui.General.Data (DimSize (..))
import Tunagui.Widget.Component.Conf (DimConf (..))

upD :: Behavior a -> Event UpdateType
upD beh = const Redraw <$> updates beh

upS :: Behavior a -> Event UpdateType
upS beh = const Reshape <$> updates beh

mkDimBehav :: (Ord a, Num a) =>
  DimSize a -> DimConf a -> Behavior a -> Reactive (Behavior a)
mkDimBehav dimA conf behContent =
  fmap work <$> case dimA of
    Absolute a -> fst <$> newBehavior a
    RelContent -> return behContent
  where
    conv f (Just x) = f x
    conv _ Nothing  = id
    --
    padding = padding1 conf + padding2 conf
    work = (+ padding) . conv max (minv conf) . conv min (maxv conf)
