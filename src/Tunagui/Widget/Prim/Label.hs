module Tunagui.Widget.Prim.Label
  (
    Label (..)
  , Config (..), defaultConfig
  , newLabelT, newLabelB
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
import Tunagui.General.Types (Point(..), Size(..), Range(..), plusPS, UpdateType)
import Tunagui.General.Base (TunaguiT, runTuna)
import Tunagui.Internal.Render as R
import Tunagui.Internal.Render.SDL (runRender)
import Tunagui.Widget.Component.Features (Renderable, render, locate, range, update)
import Tunagui.Widget.Component.Util (upS, mkSizeBehav)

data Label = Label
  { pos :: Behavior (Point Int)
  , size :: Behavior (Size Int)
  , text :: Behavior T.Text
  --
  , setPos :: Point Int -> Reactive ()
  , update_ :: Event UpdateType
  }

data Config = Config
  { width :: D.DimSize Int
  , height :: D.DimSize Int
  , minWidth :: Maybe Int
  , maxWidth :: Maybe Int
  , minHeight :: Maybe Int
  , maxHeight :: Maybe Int
  } deriving Show

defaultConfig :: Config
defaultConfig = Config
  { width = RelContent
  , height = RelContent
  , minWidth = Nothing
  , maxWidth = Nothing
  , minHeight = Nothing
  , maxHeight = Nothing
  }

instance Show Label where
  show _ = "< LABEL >"

instance Renderable Label where
  render = render_
  locate = locate_
  range  = range_
  update = update_

newLabelT :: Config -> D.Window -> T.Text -> TunaguiT Label
newLabelT c w t =
  newLabelB c w =<< toBeh t
  where
    toBeh = fmap fst . liftIO . sync . newBehavior

newLabelB :: Config -> D.Window -> Behavior T.Text -> TunaguiT Label
newLabelB cnf win behText = do
  tuna <- ask
  (behCW, pushCW) <- liftIO . sync . newBehavior $ 0
  (behCH, pushCH) <- liftIO . sync . newBehavior $ 0
  let setCW text = void . forkIO $ do
        (S (V2 w h)) <- runTuna tuna $ runRender (D.wRenderer win) (R.textSize text)
        sync $ do
          pushCW w
          pushCH h
  liftIO $ do
    setCW =<< (sync . sample) behText
    sync $ do
      listen (updates behText) setCW -- TODO: unlisten
      behW <- mkSizeBehav (width cnf) (minWidth cnf) (maxWidth cnf) behCW
      behH <- mkSizeBehav (height cnf) (minHeight cnf) (maxHeight cnf) behCH
      let behSize = S <$> (V2 <$> behW <*> behH)
      (behPos, pushPos) <- newBehavior $ P (V2 0 0)
      -- Make update event
      let eUpdate = upS behText
      return Label
        { pos = behPos
        , size = behSize
        , text = behText
        , setPos = pushPos
        , update_ = eUpdate
        }

locate_ :: Label -> Point Int -> IO ()
locate_ label = sync . setPos label

range_ :: Label -> IO (Range Int)
range_ label = sync $ do
  pos <- sample $ pos label
  size <- sample $ size label
  return $ R pos (pos `plusPS` size)

render_ :: Label -> R.RenderP TunaguiT ()
render_ label = do
  (p,t) <- liftIO . sync $
              (,) <$> sample (pos label)
                  <*> sample (text label)
  R.renderText p t
