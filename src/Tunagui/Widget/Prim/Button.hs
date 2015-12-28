module Tunagui.Widget.Prim.Button
  (
    Button (..), ButtonConfig (..)
  , defaultButtonConfig
  , newButton
  ) where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           FRP.Sodium
import           Linear.V2
import           Linear.V4
import qualified Data.Text                as T

import qualified Tunagui.General.Data     as D
import qualified Tunagui.General.Types    as T
import           Tunagui.General.Base     (TunaguiT)
import           Tunagui.Internal.Render  as R
import           Tunagui.Widget.Features  (Clickable,
                                          Renderable,
                                          onClick, render,
                                          locate)
import qualified Tunagui.Widget.Component as CMP
import           Tunagui.Widget.Layout    (DimSize (..))

data Button = Button
  { btnPos     :: Behavior (T.Point Int)
  , btnSize    :: Behavior (T.Size Int)
  -- Setter of attributes
  , setPos     :: T.Point Int -> Reactive ()
  -- Features
  , btnClkArea :: CMP.ClickableArea
  , btnText :: Maybe T.Text
  }

data ButtonConfig = ButtonConfig
  { btnWidth  :: DimSize Int
  , btnHeight :: DimSize Int
  , btnMinWidth :: Maybe Int
  , btnMaxWidth :: Maybe Int
  , btnMinHeight :: Maybe Int
  , btnMaxHeight :: Maybe Int
  , bcText :: Maybe T.Text
  } deriving Show

defaultButtonConfig :: ButtonConfig
defaultButtonConfig = ButtonConfig
  { btnWidth = RelContent
  , btnHeight = RelContent
  , btnMinWidth = Nothing
  , btnMaxWidth = Nothing
  , btnMinHeight = Nothing
  , btnMaxHeight = Nothing
  , bcText = Nothing
  }

instance Show Button where
  show _ = "< Button >"

instance Clickable Button where
  onClick = CMP.clickEvent . btnClkArea

instance Renderable Button where
  render = renderB
  locate = locateB

newButton :: ButtonConfig -> D.WinEvents -> IO Button
newButton cnf es =
  sync $ do
    (behW,_) <- case btnWidth cnf of
      Absolute x -> newBehavior x
      RelContent -> newBehavior 100 -- TODO: change to behavior accordings to contents
    (behH,_) <- case btnHeight cnf of
      Absolute x -> newBehavior x
      RelContent -> newBehavior 100
    let behW' = minW . maxW <$> behW
        behH' = minH . maxH <$> behH
    let behSize = T.S <$> (V2 <$> behW' <*> behH')
        behShape = T.Rect <$> behSize
    --
    (behPos, pushPos) <- newBehavior $ T.P (V2 0 0)
    clk <- CMP.mkClickableArea behPos behShape (D.wePML es) (D.weRML es)
    return Button
      { btnPos = behPos
      , btnSize = behSize
      , setPos = pushPos
      , btnClkArea = clk
      , btnText = bcText cnf
      }
    where
      minW = case btnMinWidth cnf of
        Just x -> min x
        Nothing -> id
      maxW = case btnMaxWidth cnf of
        Just x -> max x
        Nothing -> id
      minH = case btnMinHeight cnf of
        Just x -> min x
        Nothing -> id
      maxH = case btnMaxHeight cnf of
        Just x -> max x
        Nothing -> id

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
