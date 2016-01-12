module Tunagui.Widget.Prim.Label
  (
    Label (..)
  , Config (..), defaultConfig
  , mkLabel
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Concurrent (forkIO)
import FRP.Sodium
import qualified Data.Text as T
import Linear.V2
import Data.List (foldl1')

import qualified Graphics.UI.SDL.TTF as TTF

import qualified Tunagui.General.Data as D
import Tunagui.General.Data (DimSize(..))
import Tunagui.General.Types (Point(..), Size(..), Range(..), plusPS, mkRange)
import Tunagui.General.Base (TunaguiT, runTuna)
import Tunagui.Internal.Render as R
import Tunagui.Internal.Render (RenderT, runRender)
import Tunagui.Widget.Component.Features
import qualified Tunagui.Widget.Component.Part as PRT
import Tunagui.Widget.Component.Util (up, mkSizeBehav)
import Tunagui.Widget.Component.Conf (DimConf (..))

data Label = Label
  { render_ :: RenderT ()
  , locate_ :: Point Int -> IO ()
  , sizeof_ :: IO (Size Int)
  , updated_ :: Event ()
  , resized_ :: Event (Size Int)
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
  sizeof = sizeof_
  updated = updated_
  resized = resized_
  free = free_

mkLabel :: Config -> D.Window -> Behavior T.Text -> TunaguiT Label
mkLabel conf win behText = do
  tuna <- ask
  liftIO $ do
    font <- TTF.openFont "data/sample.ttf" 16
    sync $ do
      -- Text
      tc <- PRT.mkTextContent win font =<< (Just <$> sample behText)
      listen (updates behText) $ \text -> -- connect
        void . forkIO . void . sync $ PRT.modifyText tc (const text)
      -- Position
      (behAbsPos0, pushAbsPos0) <- newBehavior $ P (V2 0 0)
      -- Size
      (behBorderRelPos, behTextRelPos, behBorderSize, behRangeSize) <- mkSizeBehav' conf tc
      -- Make update event
      let updated' = foldl1' mappend
            [ up behText
            , up behBorderSize
            , up behRangeSize
            ]

      let render' = do
            (p, t) <- liftIO . sync $ do
              p <- sample behTextRelPos
              t <- sample $ PRT.tcText tc
              return (p, t)
            R.renderText font p t

      let free' = do
            putStrLn "free Label"
            TTF.closeFont font

      return Label
        {
          render_ = render'
        , locate_ = sync . pushAbsPos0
        , sizeof_ = sync $ sample behRangeSize
        , updated_ = updated'
        , resized_ = updates behRangeSize
        , free_ = free'
        }
  where
    mkSizeBehav' c tc =
      mkSizeBehav (width c) (widthConf c) (PRT.tcWidth tc)
                  (height c) (heightConf c) (PRT.tcHeight tc)
