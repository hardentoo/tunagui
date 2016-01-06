module Tunagui.Widget.Prim.Label
  (
    Label (..)
  , Config (..), defaultConfig
  , newLabel
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Concurrent (forkIO)
import FRP.Sodium
import qualified Data.Text as T
import Linear.V2

import qualified Tunagui.General.Data as D
import Tunagui.General.Data (DimSize(..))
import Tunagui.General.Types (Point(..), Size(..), Range(..), plusPS, mkRange, UpdateType)
import Tunagui.General.Base (TunaguiT, runTuna)
import Tunagui.Internal.Render as R
import Tunagui.Internal.Render.SDL (runRender)
import Tunagui.Widget.Component.Features
import qualified Tunagui.Widget.Component.Part as PRT
import Tunagui.Widget.Component.Util (upS, mkSizeBehav)
import Tunagui.Widget.Component.Conf (DimConf (..))

data Label = Label
  { pos :: Behavior (Point Int)
  , size :: Behavior (Size Int)
  --
  , textPos :: Behavior (Point Int)
  , text :: Behavior T.Text
  --
  , locate_ :: Point Int -> IO ()
  , update_ :: Event UpdateType
  , free_ :: IO ()
  }

data Config = Config
  { width :: D.DimSize Int
  , widthConf :: DimConf Int
  , height :: D.DimSize Int
  , heightConf :: DimConf Int
  } deriving Show

defaultConfig :: Config
defaultConfig = Config
  { width = RelContent
  , widthConf = DimConf Nothing Nothing 4 4 2 2
  , height = RelContent
  , heightConf = DimConf Nothing Nothing 4 4 2 2
  }

instance Show Label where
  show _ = "< LABEL >"

instance Renderable Label where
  render = render_
  locate = locate_
  range  = range_
  update = update_
  free   = free_

newLabel :: Config -> D.Window -> Behavior T.Text -> TunaguiT Label
newLabel conf win behText = do
  tuna <- ask
  liftIO . sync $ do
    -- Text
    tc <- PRT.mkTextContent tuna win . Just =<< sample behText
    listen (updates behText) $ \text -> -- connect
      void . forkIO . void . sync $ PRT.modifyText tc (const text)
    -- Position
    (behPos, pushPos) <- newBehavior $ P (V2 0 0)
    -- Size
    (behBorderPos, behTextPos, _behBorderSize, behRangeSize) <- mkSizeBehav' behPos conf tc
    -- Make update event
    let eUpdate = upS behText
    return Label
      { pos = behPos
      , size = behRangeSize
      -- Text
      , textPos = behTextPos
      , text = PRT.tcText tc
      -- Features
      , locate_ = sync . pushPos
      , update_ = eUpdate
      , free_ = putStrLn "free Label" -- test
      }
  where
    mkSizeBehav' behPos c tc =
      mkSizeBehav behPos (width c) (widthConf c) (PRT.tcWidth tc)
                         (height c) (heightConf c) (PRT.tcHeight tc)

range_ :: Label -> IO (Range Int)
range_ label = sync $
  mkRange <$> sample (pos label) <*> sample (size label)

render_ :: Label -> R.RenderP TunaguiT ()
render_ label = do
  (tp, t) <- liftIO . sync $ do
    tp <- sample (textPos label)
    t <- sample (text label)
    return (tp, t)
  R.renderText tp t
