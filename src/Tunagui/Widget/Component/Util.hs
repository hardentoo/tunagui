module Tunagui.Widget.Component.Util
  ( up, up'
  , mkSizeBehav
  ) where

import Control.Monad (void)
import FRP.Sodium

import Tunagui.General.Data (DimSize (..))

up :: Behavior a -> Event String
up = up' ""

up' :: String -> Behavior a -> Event String
up' str beh = const str <$> updates beh

mkSizeBehav :: Ord a => DimSize a -> Maybe a -> Maybe a -> Behavior a -> Reactive (Behavior a)
mkSizeBehav dimA minA maxA behContent = do
  behA <- case dimA of
    Absolute a -> fst <$> newBehavior a
    RelContent -> return behContent
  return $ conv min minA . conv max maxA <$> behA
  where
  conv minmax (Just x) = minmax x
  conv _      Nothing  = id
