module Tunagui.Widget.Component.Part
  (
    ClickableArea (..)
  , mkClickableArea
  ) where

import           FRP.Sodium

import qualified Tunagui.General.Types as T

data ClickableArea = ClickableArea
  { clickEvent :: Event (T.Point Int)
  }

mkClickableArea ::
  Behavior (T.Point Int) ->
  Behavior (T.Shape Int) ->
  Event (T.Point Int) ->
  Event (T.Point Int) ->
  Reactive ClickableArea
mkClickableArea bPos bShape eClick eRelease = do
  behWaitingRls <- hold False ((const True <$> eClkOn) `merge` (const False <$> eRelease))
  let eClk = (fst . fst) <$> filterE snd (snapshot (,) eRlsOn behWaitingRls)
  return $ ClickableArea eClk
  where
    within' = uncurry T.within
    bArea = (,) <$> bPos <*> bShape
    eClkOn = filterE within' $ snapshot (,) eClick bArea
    eRlsOn = filterE within' $ snapshot (,) eRelease bArea
