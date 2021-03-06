{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tunagui.Internal.Render where

import           Control.Exception         (bracket, bracket_)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (ReaderT, MonadReader, runReaderT, ask)
import           Data.Word                 (Word8)
import           Foreign.C.Types
import           Linear.V2
import           Linear.V4
import qualified Linear.Affine             as A
import qualified Data.Text                 as T
import           Control.Monad.Managed     (runManaged, managed)

import qualified SDL
import           SDL                       (($=), get)
import           SDL.Raw (Color(..))
import qualified Graphics.UI.SDL.TTF as TTF
import           Graphics.UI.SDL.TTF.FFI (TTFFont)

import           Tunagui.General.Types     (Point(..), Size(..))

newtype RenderT a = RenderT {
    runR :: ReaderT SDL.Renderer IO a
  } deriving (Functor, Applicative,
              Monad, MonadIO,
              MonadReader SDL.Renderer)

runRender :: SDL.Renderer -> RenderT a -> IO a
runRender renderer k = runReaderT (runR k) renderer

clear :: RenderT ()
clear = ask >>= SDL.clear

flush :: RenderT ()
flush = ask >>= SDL.present

onTexture :: SDL.Texture -> RenderT a -> RenderT a
onTexture texture f = do
  r <- ask
  liftIO $ do
    curTarget <- get $ SDL.rendererRenderTarget r
    bracket_ (SDL.rendererRenderTarget r $= Just texture)
             (SDL.rendererRenderTarget r $= curTarget)
             (runRender r f)

createTexture :: V2 Int -> RenderT SDL.Texture
createTexture size = do
  r <- ask
  SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget (fromIntegral <$> size)

destroyTexture :: MonadIO m => SDL.Texture -> m ()
destroyTexture = SDL.destroyTexture

withTexture :: V2 Int -> (SDL.Texture -> RenderT a) -> RenderT a
withTexture size f = do
  r <- ask
  liftIO $
    bracket (runRender r $ createTexture size)
            destroyTexture
            (runRender r . f)

copy ::
  SDL.Texture
  -> Maybe (SDL.Rectangle CInt)
  -> Maybe (SDL.Rectangle CInt)
  -> RenderT ()
copy tex mr1 mr2 = do
  r <- ask
  SDL.copy r tex mr1 mr2

setColor :: V4 Word8 -> RenderT ()
setColor color = do
  r <- ask
  SDL.rendererDrawColor r $= color

fillRect :: Point Int -> Size Int -> RenderT ()
fillRect p s = do
  r <- ask
  SDL.fillRect r $ Just (SDL.Rectangle (convP p) (convS s))

drawRect :: Point Int -> Size Int -> RenderT ()
drawRect p s = do
  r <- ask
  SDL.drawRect r $ Just (SDL.Rectangle (convP p) (convS s))

textSize :: TTFFont -> T.Text -> RenderT (Size Int)
textSize font text = liftIO $ do
  (w, h) <- TTF.sizeText font (T.unpack text)
  return $ S (V2 w h)

renderText :: TTFFont -> Point Int -> T.Text -> RenderT ()
renderText font pos text = do
  r <- ask
  (S sz) <- textSize font text
  liftIO . runManaged $ do
    surface <- managed $ bracket (mkSurface <$> TTF.renderTextBlended font (T.unpack text) (Color 255 255 255 255)) SDL.freeSurface
    texture <- managed $ bracket (SDL.createTextureFromSurface r surface) SDL.destroyTexture
    let rect = Just $ SDL.Rectangle pos' (fromIntegral <$> sz)
    SDL.copy r texture Nothing rect
  where
    (P p) = pos
    pos' = A.P $ fromIntegral <$> p
    mkSurface ptr = SDL.Surface ptr Nothing

-- *****************************************************************************
convP :: (Integral a, Integral b) => Point a -> A.Point V2 b
convP (P p) = A.P $ fromIntegral <$> p

convS :: (Integral a, Integral b) => Size a -> V2 b
convS (S s) = fromIntegral <$> s
