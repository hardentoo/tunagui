module Tunagui.Widget.Component.Part
  (
    ClickableArea (..)
  , mkClickableArea
  , TextContent (..)
  , mkTextContent
  ) where

import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (forkIO)
import           FRP.Sodium
import qualified Data.Text as T
import           Linear.V2
import           Data.Maybe (fromMaybe)

import           Tunagui.General.Base (Tunagui, runTuna)
import qualified Tunagui.General.Data as D
import           Tunagui.General.Types (Point(..), Size(..), Shape(..), within)
import qualified Tunagui.Internal.Render as R
import           Tunagui.Internal.Render.SDL (runRender)

data ClickableArea = ClickableArea
  { clickEvent :: Event (Point Int)
  , crossBoundary :: Event Bool
  }

mkClickableArea ::
  Behavior (Point Int) ->
  Behavior (Shape Int) ->
  Event (Point Int) ->
  Event (Point Int) ->
  Event (Point Int) -> -- Mouse motion
  Reactive ClickableArea
mkClickableArea bPos bShape eClick eRelease eMotion = do
  behWaitingRls <- hold False ((const True <$> eClkOn) `merge` (const False <$> eRelease))
  let eClk = (fst . fst) <$> filterE snd (snapshot (,) eRlsOn behWaitingRls)
  ClickableArea
    <$> pure eClk
    <*> mkMotion
  where
    within' = uncurry within
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

data TextContent = TextContent
  { tcText :: Behavior T.Text
  , tcWidth :: Behavior Int
  , tcHeight :: Behavior Int
  --
  , modifyText :: (T.Text -> T.Text) -> Reactive ()
  }

mkTextContent :: Tunagui -> D.Window -> Maybe T.Text -> Reactive TextContent
mkTextContent tuna win mtext = do
  (behCW, pushCW) <- newBehavior 0
  (behCH, pushCH) <- newBehavior 0
  (behText, pushText) <- newBehavior $ T.pack ""
  listen (updates behText) $ \text ->
    void . forkIO . runTuna tuna $ do
      (S (V2 w h)) <- runRender (D.wRenderer win) (R.textSize text)
      liftIO . sync $ do
        pushCW w
        pushCH h
  pushText $ fromMaybe (T.pack "") mtext
  return TextContent
    { tcText = behText
    , tcWidth = behCW
    , tcHeight = behCH
    --
    , modifyText = \f -> pushText . f =<< sample behText
    }
