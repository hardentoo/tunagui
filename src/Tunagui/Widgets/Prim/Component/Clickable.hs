module Tunagui.Widgets.Prim.Component.Clickable where
  -- (
  -- ) where

import Control.Applicative
import FRP.Sodium

import qualified Tunagui.General.Types as T

data Clickable = Clickable
  { clkClickEvent :: Event (T.Point Int)
  }

mkClickable ::
  Behavior (T.Shape Int) ->
  Event (T.Point Int) ->
  Event (T.Point Int) ->
  IO Clickable
mkClickable bShape eClick eRelease = sync $ do
  behWaitingRls <- hold False ((const True <$> eClkOn) `merge` (const False <$> eRelease))
  let eClk = (fst . fst) <$> filterE snd (snapshot (,) eRlsOn behWaitingRls)
  return $ Clickable eClk
  where
    within' = uncurry T.within
    eClkOn = filterE within' $ snapshot (,) eClick bShape
    eRlsOn = filterE within' $ snapshot (,) eRelease bShape
