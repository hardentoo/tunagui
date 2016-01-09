module Tunagui.Widget.Component.Util
  ( up
  , mkSizeBehav
  ) where

import Control.Monad (void)
import FRP.Sodium
import Linear.V2

import Tunagui.General.Types (Point (..), Size (..), plusPP)
import Tunagui.General.Data (DimSize (..))
import Tunagui.Widget.Component.Conf (DimConf (..))

up :: Behavior a -> Event ()
up beh = void $ updates beh

mkDimBehav :: (Ord a, Num a) =>
  DimSize a ->
  DimConf a ->
  Behavior a ->
  Reactive (Behavior a, Behavior a)
mkDimBehav dimA conf behContent = do
  behBorder <- fmap minmax <$> case dimA of
    Absolute a -> fst <$> newBehavior a
    RelContent -> return $ (+ padding) <$> behContent
  let behRange = (+ margin) <$> behBorder
  return (behBorder, behRange)
  where
    conv f (Just x) = f x
    conv _ Nothing  = id
    --
    padding = padding1 conf + padding2 conf
    margin = margin1 conf + margin2 conf
    minmax = conv max (minv conf) . conv min (maxv conf)

mkSizeBehav :: (Ord a, Num a) =>
  DimSize a -> DimConf a -> Behavior a -> -- Width
  DimSize a -> DimConf a -> Behavior a -> -- Height
  Reactive (Behavior (Point a), Behavior (Point a), Behavior (Size a), Behavior (Size a))
mkSizeBehav wDim wConf behCW hDim hConf behCH = do
  (behBW, behRW) <- mkDimBehav wDim wConf behCW
  (behBH, behRH) <- mkDimBehav hDim hConf behCH
  let behBorderSize = mkSize behBW behBH
      behRangeSize = mkSize behRW behRH
  behBorderRelPos <- fst <$> newBehavior margin
  behContentRelPos <- fst <$> newBehavior (margin `plusPP` padding)
  return (behBorderRelPos, behContentRelPos, behBorderSize, behRangeSize)
  where
    mkSize w h = S <$> (V2 <$> w <*> h)
    margin = P (V2 (margin1 wConf) (margin1 hConf))
    padding = P (V2 (padding1 wConf) (padding1 hConf))
