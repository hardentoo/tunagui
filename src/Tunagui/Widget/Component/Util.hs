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

mkSizeBehav :: Ord a => DimSize a -> Maybe a -> Maybe a -> Behavior a -> Reactive (Behavior a)
mkSizeBehav dimA minA maxA behContent = do
  behA <- case dimA of
    Absolute a -> fst <$> newBehavior a
    RelContent -> return behContent
  return $ conv min minA . conv max maxA <$> behA
  where
  conv minmax (Just x) = minmax x
  conv _      Nothing  = id
