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
mkSizeBehav dimA minA maxA behContent =
  fmap work <$> case dimA of
    Absolute a -> fst <$> newBehavior a
    RelContent -> return behContent
  where
    conv f (Just x) = f x
    conv _ Nothing  = id
    --
    work = conv min minA . conv max maxA
