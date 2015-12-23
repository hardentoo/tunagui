module Tunagui.Widgets.Prim.Button
  (
    Button (..)
  , newButton
  ) where

import           Control.Monad.Reader                         (asks)
import           FRP.Sodium
import           Linear.V2


import qualified Tunagui.General.Data                         as D
import qualified Tunagui.General.Types                        as T
import           Tunagui.Internal.Base
import qualified Tunagui.Widgets.Prim.Component.ClickableArea as CLK

import           Tunagui.Widgets.Features                     (Clickable,
                                                               onClick)

data Button = Button {
    btnClkArea :: CLK.ClickableArea
  }

instance Clickable Button where
  onClick = CLK.clickEvent . btnClkArea

newButton :: Base Button
newButton = do
  es <- asks D.cntEvents
  liftIO $ do
    (behPos,_) <- sync . newBehavior $ T.P (V2 0 0)
    (behSize,_) <- sync . newBehavior $ T.S (V2 widgh height)
    let behShape = T.Rect <$> behSize
    clk <- CLK.mkClickableArea behPos behShape (D.ePML es) (D.eRML es)
    return $ Button clk
  where
    widgh = 100
    height = 50

-- freeButton :: Button -> IO ()
-- freeButton button = do
--   putStrLn "Add code freeing Button here."
--   return ()
