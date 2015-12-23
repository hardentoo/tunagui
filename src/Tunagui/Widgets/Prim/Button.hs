module Tunagui.Widgets.Prim.Button
  (
    Button (..)
  , newButton
  , onClickButton
  ) where

import Control.Monad.Reader (asks)
import FRP.Sodium
import Linear.V2

import qualified Tunagui.General.Data as D
import qualified Tunagui.General.Types as T
import Tunagui.Internal.Base
import qualified Tunagui.Widgets.Prim.Component.Clickable as CLK

data Button = Button
  {
    btnClk :: CLK.Clickable
  }

newButton :: Base Button
newButton = do
  es <- asks D.cntEvents
  liftIO $ do
    let shape = mkShape
    (behShape,_) <- sync $ newBehavior shape
    clk <- CLK.mkClickable behShape (D.ePML es) (D.eRML es)
    return $ Button clk
  where
    mkShape = T.Rect (T.P (V2 0 0)) (T.S (V2 100 100))

-- freeButton :: Button -> IO ()
-- freeButton button = do
--   putStrLn "Add code freeing Button here."
--   return ()

onClickButton :: Button -> Event (T.Point Int)
onClickButton = CLK.clkClickEvent . btnClk
