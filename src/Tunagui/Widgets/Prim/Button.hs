module Tunagui.Widgets.Prim.Button
  (
    Button (..), ButtonConfig (..)
  , newButton
  ) where

import           Control.Monad.IO.Class                       (MonadIO)
import           Control.Monad.Reader                         (asks)
import           FRP.Sodium
import           Linear.V2
import qualified SDL


import qualified Tunagui.General.Data                         as D
import qualified Tunagui.General.Types                        as T
import           Tunagui.Internal.Base
import qualified Tunagui.Widgets.Prim.Component.ClickableArea as CLK

import           Tunagui.Internal.Operation.Render            (RenderP)
import           Tunagui.Internal.Operation.Render.SDL        as R
import           Tunagui.Widgets.Features                     (Clickable,
                                                               Renderable,
                                                               onClick, render)

-- TODO: Hide 'newButton' from user

data Button = Button
  { btnPos     :: Behavior (T.Point Int)
  , btnSize    :: Behavior (T.Size Int)
  -- Features
  , btnClkArea :: CLK.ClickableArea
  }

data ButtonConfig = ButtonConfig
  { btnWidth  :: Int
  , btnHeight :: Int
  }

instance Show Button where
  show _ = "< Button >"

instance Clickable Button where
  onClick = CLK.clickEvent . btnClkArea

instance Renderable Button where
  render = renderB

newButton :: ButtonConfig -> Base Button
newButton cfg = do
  es <- asks D.cntEvents
  liftIO $ do
    (behPos,_) <- sync . newBehavior $ T.P (V2 0 0)
    (behSize,_) <- sync . newBehavior $ iniSize
    let behShape = T.Rect <$> behSize
    clk <- CLK.mkClickableArea behPos behShape (D.ePML es) (D.eRML es)
    return $ Button behPos behSize clk
  where
    w = btnWidth cfg
    h = btnHeight cfg
    iniSize = T.S (V2 w h)

-- freeButton :: Button -> IO ()
-- freeButton button = do
--   putStrLn "Add code freeing Button here."
--   return ()

renderB :: MonadIO m => Button -> RenderP m ()
renderB btn = do
  (p,s) <- liftIO . sync $ (,) <$> sample (btnPos btn) <*> sample (btnSize btn)
  R.drawRect p s
