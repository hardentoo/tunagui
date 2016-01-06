module Tunagui.Widget.Prim.Button
  (
    Button (..), Config (..)
  , defaultConfig
  , newButton
  ) where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (ask)
import           Control.Monad            (void)
import           Control.Concurrent       (forkIO)
import           FRP.Sodium
import           Linear.V2
import           Linear.V4
import qualified Data.Text                as T
import           Data.List                (foldl1')
import           Data.Maybe               (fromMaybe)

import qualified Tunagui.General.Data     as D
import           Tunagui.General.Data     (DimSize (..))
import           Tunagui.General.Types    (Point(..), Size(..), Range(..), Shape(..), plusPS, mkRange, UpdateType)
import           Tunagui.General.Base     (TunaguiT, runTuna)
import           Tunagui.Internal.Render  as R
import           Tunagui.Internal.Render.SDL (runRender)
import           Tunagui.Widget.Component.Features
import qualified Tunagui.Widget.Component.Part as PRT
import           Tunagui.Widget.Component.Util (upS, upD, mkSizeBehav)
import           Tunagui.Widget.Component.Color as COL
import           Tunagui.Widget.Component.Conf (DimConf (..))

data Button = Button
  { pos :: Behavior (Point Int)
  , size :: Behavior (Size Int)
  , padding :: Behavior (Size Int)
  , color :: Behavior COL.ShapeColor
  -- Text
  , text :: Behavior T.Text
  , modifyText :: (T.Text -> T.Text) -> Reactive ()
  -- Features
  , btnClkArea :: PRT.ClickableArea
  , locate_     :: Point Int -> IO ()
  , update_ :: Event UpdateType
  , free_ :: IO ()
  }

data Config = Config
  { width  :: DimSize Int
  , widthConf :: DimConf Int
  , height :: DimSize Int
  , heightConf :: DimConf Int
  --
  , bcText :: Maybe T.Text
  } deriving Show

defaultConfig :: Config
defaultConfig = Config
  { width = RelContent
  , widthConf = DimConf Nothing Nothing 4 4 2 2
  , height = RelContent
  , heightConf = DimConf Nothing Nothing 2 2 2 2
  --
  , bcText = Nothing
  }

instance Show Button where
  show _ = "< BUTTON >"

instance Clickable Button where
  clickEvent = PRT.clickEvent . btnClkArea

instance Renderable Button where
  render = render_
  locate = locate_
  range  = range_
  update = update_
  free   = free_

newButton :: Config -> D.Window -> TunaguiT Button
newButton c win = do
  tuna <- ask
  liftIO . sync $ do
    -- Text
    tc <- PRT.mkTextContent tuna win (bcText c)
    -- Position
    (behPos, pushPos) <- newBehavior $ P (V2 0 0)
    -- Size
    behSize <- mkSize c tc
    -- Padding
    behPadding <- mkPadding c
    -- Make parts
    clk <- PRT.mkClickableArea behPos (Rect <$> behSize) (D.wePML events) (D.weRML events) (D.weMMPos events)
    -- Hover
    behShapeColor <- hold COL.planeShapeColor $ toShapeColor <$> PRT.crossBoundary clk
    -- Update event
    let eUpdate = foldl1' mappend [upS behPos, upS behSize, upD behShapeColor]
    return Button
      { pos = behPos
      , size = behSize
      , padding = behPadding
      , color = behShapeColor
      , btnClkArea = clk
      -- Text
      , text = PRT.tcText tc
      , modifyText = PRT.modifyText tc
      --
      , locate_ = sync . pushPos
      , update_ = eUpdate
      , free_ = putStrLn "free Button" -- test
      }
  where
    events = D.wEvents win
    toShapeColor :: Bool -> COL.ShapeColor
    toShapeColor True  = COL.hoverShapeColor
    toShapeColor False = COL.planeShapeColor

    mkSize c tc = do
      behW <- mkSizeBehav (width c) (widthConf c) (PRT.tcWidth tc)
      behH <- mkSizeBehav (height c) (heightConf c) (PRT.tcHeight tc)
      return $ S <$> (V2 <$> behW <*> behH)

    mkPadding c = do
      behPaddingLeft <- fst <$> newBehavior (padding1 (widthConf c))
      behPaddingTop <- fst <$> newBehavior (padding1 (heightConf c))
      return $ S <$> (V2 <$> behPaddingLeft <*> behPaddingTop)

range_ :: Button -> IO (Range Int)
range_ btn = sync $
  mkRange <$> sample (pos btn) <*> sample (size btn)

render_ :: Button -> R.RenderP TunaguiT ()
render_ btn = do
  (p, s, pd, c, t) <- liftIO . sync $ do
    p <- sample $ pos btn
    s <- sample $ size btn
    pd <- sample $ padding btn
    c <- sample $ color btn
    t <- sample $ text btn
    return (p, s, pd, c, t)
  R.setColor $ COL.fill c
  R.fillRect p s
  R.setColor $ COL.border c
  R.drawRect p s
  -- Text
  R.renderText (p `plusPS` pd) t
