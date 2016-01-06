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
import Tunagui.Widget.Component.Features
import qualified Tunagui.Widget.Component.Part as PRT
import Tunagui.Widget.Component.Util (upS, mkSizeBehav)

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
  , height :: D.DimSize Int
  -- Boundary
  , minWidth :: Maybe Int
  , maxWidth :: Maybe Int
  , minHeight :: Maybe Int
  , maxHeight :: Maybe Int
  -- Padding
  , paddingLeft :: Int
  , paddingRight :: Int
  , paddingTop :: Int
  , paddingBottom :: Int
  } deriving Show

defaultConfig :: Config
defaultConfig = Config
  { width = RelContent
  , height = RelContent
  --
  , minWidth = Nothing
  , maxWidth = Nothing
  , minHeight = Nothing
  , maxHeight = Nothing
  --
  , paddingLeft   = 10
  , paddingRight  = 10
  , paddingTop    = 10
  , paddingBottom = 10
  }

instance Show Label where
  show _ = "< LABEL >"

instance Renderable Label where
  render = render_
  locate = locate_
  range  = range_
  update = update_
  free   = free_

newLabelT :: Config -> D.Window -> T.Text -> TunaguiT Label
newLabelT c w t =
  newLabelB c w =<< toBeh t
  where
    toBeh = fmap fst . liftIO . sync . newBehavior

newLabelB :: Config -> D.Window -> Behavior T.Text -> TunaguiT Label
newLabelB cnf win behText = do
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
      behW <- mkSizeBehav (width cnf) (minWidth cnf) (maxWidth cnf) (paddingLeft cnf) (paddingRight cnf) (PRT.tcWidth tc)
      behH <- mkSizeBehav (height cnf) (minHeight cnf) (maxHeight cnf) (paddingTop cnf) (paddingBottom cnf) (PRT.tcHeight tc)
      return $ S <$> (V2 <$> behW <*> behH)

    mkPadding cnf = do
      behPaddingLeft <- fst <$> newBehavior (paddingLeft cnf)
      behPaddingRight <- fst <$> newBehavior (paddingRight cnf)
      return $ S <$> (V2 <$> behPaddingLeft <*> behPaddingRight)

range_ :: Label -> IO (Range Int)
range_ label = sync $ do
  pos <- sample $ pos label
  size <- sample $ size label
  return $ R pos (pos `plusPS` size)

render_ :: Label -> R.RenderP TunaguiT ()
render_ label = do
  (p, t) <- liftIO . sync $ do
    p <- sample (pos label)
    t <- sample (text label)
    pd <- sample (padding label)
    return (p `plusPS` pd, t)
  R.renderText p t
