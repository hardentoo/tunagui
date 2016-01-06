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
import           Tunagui.General.Types    (Point(..), Size(..), Range(..), Shape(..), plusPS, UpdateType)
import           Tunagui.General.Base     (TunaguiT, runTuna)
import           Tunagui.Internal.Render  as R
import           Tunagui.Internal.Render.SDL (runRender)
import           Tunagui.Widget.Component.Features
import qualified Tunagui.Widget.Component.Part as PRT
import           Tunagui.Widget.Component.Util (upS, upD, mkSizeBehav)
import           Tunagui.Widget.Component.Color as COL

data Button = Button
  { btnPos     :: Behavior (Point Int)
  , btnSize    :: Behavior (Size Int)
  , btnPadding  :: Behavior (Size Int)
  , btnColor   :: Behavior COL.ShapeColor
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
  { bcWidth  :: DimSize Int
  , bcHeight :: DimSize Int
  -- Boundary
  , bcMinWidth :: Maybe Int
  , bcMaxWidth :: Maybe Int
  , bcMinHeight :: Maybe Int
  , bcMaxHeight :: Maybe Int
  -- Padding
  , bcPaddingLeft :: Int
  , bcPaddingRight :: Int
  , bcPaddingTop :: Int
  , bcPaddingBottom :: Int
  --
  , bcText :: Maybe T.Text
  } deriving Show

defaultConfig :: Config
defaultConfig = Config
  { bcWidth = RelContent
  , bcHeight = RelContent
  --
  , bcMinWidth = Nothing
  , bcMaxWidth = Nothing
  , bcMinHeight = Nothing
  , bcMaxHeight = Nothing
  --
  , bcPaddingLeft   = 10
  , bcPaddingRight  = 10
  , bcPaddingTop    = 10
  , bcPaddingBottom = 10
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
    -- (behCW, behCH, behText, pushText) <- mkText tuna $ bcText c
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
      { btnPos = behPos
      , btnSize = behSize
      , btnPadding = behPadding
      , btnColor = behShapeColor
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
      behW <- mkSizeBehav (bcWidth c) (bcMinWidth c) (bcMaxWidth c) (bcPaddingLeft c) (bcPaddingRight c) (PRT.tcWidth tc)
      behH <- mkSizeBehav (bcHeight c) (bcMinHeight c) (bcMaxHeight c) (bcPaddingTop c) (bcPaddingBottom c) (PRT.tcHeight tc)
      return $ S <$> (V2 <$> behW <*> behH)

    mkPadding c = do
      behPaddingLeft <- fst <$> newBehavior (bcPaddingLeft c)
      behPaddingTop <- fst <$> newBehavior (bcPaddingTop c)
      return $ S <$> (V2 <$> behPaddingLeft <*> behPaddingTop)

range_ :: Button -> IO (Range Int)
range_ btn = sync $ do
  pos <- sample (btnPos btn)
  size <- sample (btnSize btn)
  return $ R pos (pos `plusPS` size)

render_ :: Button -> R.RenderP TunaguiT ()
render_ btn = do
  (p, s, pWithPad, color, t) <- liftIO . sync $ do
    p <- sample $ btnPos btn
    s <- sample $ btnSize btn
    pd <- sample $ btnPadding btn
    color <- sample $ btnColor btn
    t <- sample $ text btn
    return (p, s, p `plusPS` pd, color, t)
  R.setColor $ COL.fill color
  R.fillRect p s
  R.setColor $ COL.border color
  R.drawRect p s
  -- Text
  R.renderText pWithPad t
