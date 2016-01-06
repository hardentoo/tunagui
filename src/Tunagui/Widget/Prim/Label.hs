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
  , padding :: Behavior (Size Int)
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
  , widthConf = DimConf Nothing Nothing 10 10 0 0
  , height = RelContent
  , heightConf = DimConf Nothing Nothing 10 10 0 0
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
newLabel cnf win behText = do
  tuna <- ask
  liftIO . sync $ do
    -- Text
    tc <- PRT.mkTextContent tuna win . Just =<< sample behText
    listen (updates behText) $ \text -> -- connect
      void . forkIO . void . sync $ PRT.modifyText tc (const text)
    -- Position
    (behPos, pushPos) <- newBehavior $ P (V2 0 0)
    -- Size
    behSize <- mkSize cnf tc
    -- Padding
    behPadding <- mkPadding cnf
    -- Make update event
    let eUpdate = upS behText
    return Label
      { pos = behPos
      , size = behSize
      , padding = behPadding
      , text = PRT.tcText tc
      , locate_ = sync . pushPos
      , update_ = eUpdate
      , free_ = putStrLn "free Label" -- test
      }
  where
    mkSize cnf tc = do
      behW <- mkSizeBehav (width cnf) (widthConf cnf) (PRT.tcWidth tc)
      behH <- mkSizeBehav (height cnf) (heightConf cnf) (PRT.tcHeight tc)
      return $ S <$> (V2 <$> behW <*> behH)

    mkPadding cnf = do
      behPaddingLeft <- fst <$> newBehavior (padding1 (widthConf cnf))
      behPaddingRight <- fst <$> newBehavior (padding1 (heightConf cnf))
      return $ S <$> (V2 <$> behPaddingLeft <*> behPaddingRight)

range_ :: Label -> IO (Range Int)
range_ label = sync $
  mkRange <$> sample (pos label) <*> sample (size label)

render_ :: Label -> R.RenderP TunaguiT ()
render_ label = do
  (p, t) <- liftIO . sync $ do
    p <- sample (pos label)
    t <- sample (text label)
    pd <- sample (padding label)
    return (p `plusPS` pd, t)
  R.renderText p t
