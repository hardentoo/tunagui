module Tunagui.Widgets.Prim.Component.ClickableArea
  (
    ClickableArea (..)
  , mkClickableArea
  ) where

import           Control.Applicative
import           FRP.Sodium

import qualified Tunagui.General.Types as T

data ClickableArea = ClickableArea
  { clickEvent :: Event (T.Point Int)
  }

mkClickableArea ::
  Behavior (T.Shape Int) ->
  Event (T.Point Int) ->
  Event (T.Point Int) ->
  IO ClickableArea
mkClickableArea bShape eClick eRelease = sync $ do
  behWaitingRls <- hold False ((const True <$> eClkOn) `merge` (const False <$> eRelease))
  let eClk = (fst . fst) <$> filterE snd (snapshot (,) eRlsOn behWaitingRls)
  return $ ClickableArea eClk
  where
    within' = uncurry T.within
    eClkOn = filterE within' $ snapshot (,) eClick bShape
    eRlsOn = filterE within' $ snapshot (,) eRelease bShape
