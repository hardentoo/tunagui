module Tunagui.Widget.Component.Part
  (
    ClickableArea (..)
  , mkClickableArea
  ) where

import           Control.Monad (void)
import           Control.Concurrent (forkIO)
import           FRP.Sodium

import qualified Tunagui.General.Types as T

data ClickableArea = ClickableArea
  { clickEvent :: Event (T.Point Int)
  , crossBoundary :: Event Bool
  }

mkClickableArea ::
  Behavior (T.Point Int) ->
  Behavior (T.Shape Int) ->
  Event (T.Point Int) ->
  Event (T.Point Int) ->
  Event (T.Point Int) -> -- Mouse motion
  Reactive ClickableArea
mkClickableArea bPos bShape eClick eRelease eMotion = do
  behWaitingRls <- hold False ((const True <$> eClkOn) `merge` (const False <$> eRelease))
  let eClk = (fst . fst) <$> filterE snd (snapshot (,) eRlsOn behWaitingRls)
  ClickableArea
    <$> pure eClk
    <*> mkMotion
  where
    within' = uncurry T.within
    bArea = (,) <$> bPos <*> bShape
    -- Click
    eClkOn = filterE within' $ snapshot (,) eClick bArea
    eRlsOn = filterE within' $ snapshot (,) eRelease bArea
    -- Motion
    mkMotion = do
      (behPre, pushPre) <- newBehavior False
      listen eMotionOn $ void . forkIO . sync . pushPre
      return $ fst <$> filterE (uncurry (/=)) (snapshot (,) eMotionOn behPre)
      where
        eMotionOn = within' <$> snapshot (,) eMotion bArea
