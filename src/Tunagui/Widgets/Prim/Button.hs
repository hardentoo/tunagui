module Tunagui.Widgets.Prim.Button
  (
    Button (..), ButtonConfig (..)
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

-- TODO: Hide 'newButton' from user

data Button = Button
  { btnClkArea :: CLK.ClickableArea
  }

data ButtonConfig = ButtonConfig
  { btnSize :: T.Size Int
  }


instance Clickable Button where
  onClick = CLK.clickEvent . btnClkArea

newButton :: ButtonConfig -> Base Button
newButton cfg = do
  es <- asks D.cntEvents
  liftIO $ do
    (behPos,_) <- sync . newBehavior $ T.P (V2 0 0)
    (behSize,_) <- sync . newBehavior $ btnSize cfg
    let behShape = T.Rect <$> behSize
    clk <- CLK.mkClickableArea behPos behShape (D.ePML es) (D.eRML es)
    return $ Button clk

-- freeButton :: Button -> IO ()
-- freeButton button = do
--   putStrLn "Add code freeing Button here."
--   return ()
