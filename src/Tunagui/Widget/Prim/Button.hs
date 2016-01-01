module Tunagui.Widget.Prim.Button
  (
    Button (..), Config (..)
  , defaultConfig
  , newButton
  ) where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           FRP.Sodium
import           Linear.V2
import           Linear.V4
import qualified Data.Text                as T
import           Data.List                (foldl1')

import qualified Tunagui.General.Data     as D
import           Tunagui.General.Data     (DimSize (..))
import qualified Tunagui.General.Types    as T -- TODO: stop qualified
import           Tunagui.General.Base     (TunaguiT)
import           Tunagui.Internal.Render  as R
import           Tunagui.Internal.Render.SDL (runRender)
import           Tunagui.Widget.Component.Features  (Clickable,
                                          Renderable,
                                          onClick, render,
                                          locate, update)
import qualified Tunagui.Widget.Component.Part as PRT
import           Tunagui.Widget.Component.Util (up', mkSizeBehav)

data Button = Button
  { btnPos     :: Behavior (T.Point Int)
  , btnSize    :: Behavior (T.Size Int)
  -- Setter of attributes
  , setPos     :: T.Point Int -> Reactive ()
  -- Features
  , btnClkArea :: PRT.ClickableArea
  , btnText :: Maybe T.Text -- TODO: Behavior Text
  , update_ :: Event String
  }

data Config = Config
  { bcWidth  :: DimSize Int
  , bcHeight :: DimSize Int
  , bcMinWidth :: Maybe Int
  , bcMaxWidth :: Maybe Int
  , bcMinHeight :: Maybe Int
  , bcMaxHeight :: Maybe Int
  , bcText :: Maybe T.Text
  } deriving Show

defaultConfig :: Config
defaultConfig = Config
  { bcWidth = RelContent
  , bcHeight = RelContent
  , bcMinWidth = Nothing
  , bcMaxWidth = Nothing
  , bcMinHeight = Nothing
  , bcMaxHeight = Nothing
  , bcText = Nothing
  }

instance Show Button where
  show _ = "< Button >"

instance Clickable Button where
  onClick = PRT.clickEvent . btnClkArea

instance Renderable Button where
  render = renderB
  locate = locateB
  update = update_

newButton :: Config -> D.Window -> TunaguiT Button
newButton c win = do
  -- Text size
  (T.S (V2 contW contH)) <- case bcText c of
    Just text -> runRender (D.wRenderer win) (R.textSize text)
    Nothing   -> return (T.S (V2 10 10))

  liftIO . sync $ do
    (behCW, _changeCW) <- newBehavior contW -- TODO: Call changeCW when content was changed
    (behCH, _changeCH) <- newBehavior contH
    behW <- mkSizeBehav (bcWidth c) (bcMinWidth c) (bcMaxWidth c) behCW
    behH <- mkSizeBehav (bcHeight c) (bcMinHeight c) (bcMaxHeight c) behCH
    let behSize = T.S <$> (V2 <$> behW <*> behH)
        behShape = T.Rect <$> behSize
    --
    (behPos, pushPos) <- newBehavior $ T.P (V2 0 0)
    clk <- PRT.mkClickableArea behPos behShape (D.wePML events) (D.weRML events)
    -- Update event
    let eUpdate = foldl1' mappend [up' "Button.behPos" behPos, up' "Button.behSize" behSize]
    return Button
      { btnPos = behPos
      , btnSize = behSize
      , setPos = pushPos
      , btnClkArea = clk
      , btnText = bcText c
      , update_ = eUpdate
      }
  where
    events = D.wEvents win

locateB :: Button -> T.Point Int -> Reactive (T.Range Int)
locateB btn p = do
  setPos btn p
  pos <- sample (btnPos btn)
  size <- sample (btnSize btn)
  return $ T.R pos (pos `T.plusPS` size)

renderB :: Button -> R.RenderP TunaguiT ()
renderB btn = do
  (p,s) <- liftIO . sync $ (,) <$> sample (btnPos btn) <*> sample (btnSize btn)
  R.setColor $ V4 255 255 255 255
  R.fillRect p s
  R.setColor $ V4 137 140 149 255
  R.drawRect p s
  --
  case btnText btn of
    Just text -> R.renderText p text
    Nothing   -> return ()
