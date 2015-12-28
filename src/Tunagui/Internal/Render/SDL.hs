{-# LANGUAGE GADTs #-}

module Tunagui.Internal.Render.SDL
  (
    runRender
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Control.Exception (bracket)
import           Control.Monad.Operational
import           Linear.V2
import           Linear.V4
import qualified Linear.Affine as A
import           Control.Monad.Managed (runManaged, managed)

import           SDL                             (($=))
import qualified SDL
import qualified SDL.Font as TTF

import qualified Tunagui.General.Types as T
import           Tunagui.General.Base (Tunagui (..), TunaguiT)
import           Tunagui.Internal.Render

runRender :: SDL.Renderer -> RenderP TunaguiT a -> TunaguiT ()
runRender = interpret

interpret :: SDL.Renderer -> RenderP TunaguiT a -> TunaguiT ()
interpret r is = eval r =<< viewT is

convP :: (Integral a, Integral b) => T.Point a -> A.Point V2 b
convP (T.P p) = A.P $ fromIntegral <$> p

convS :: (Integral a, Integral b) => T.Size a -> V2 b
convS (T.S s) = fromIntegral <$> s

eval :: SDL.Renderer -> ProgramViewT RenderI TunaguiT a -> TunaguiT ()
eval _ (Return _) = return ()

-- Basic
eval r (Clear :>>= is) = SDL.clear r >> interpret r (is ())
eval r (Flush :>>= is) = SDL.present r >> interpret r (is ())
eval r (SetColor c :>>= is) = do
  SDL.rendererDrawColor r $= c
  interpret r (is ())

-- Rect
eval r (FillRect p s :>>= is) = do
  SDL.fillRect r $ Just (SDL.Rectangle (convP p) (convS s))
  interpret r (is ())
eval r (DrawRect p s :>>= is) = do
  SDL.drawRect r $ Just (SDL.Rectangle (convP p) (convS s))
  interpret r (is ())

-- Text
eval r (RenderText (T.P p) text :>>= is) = do
  font <- asks cntFont
  liftIO . runManaged $ do
    surface <- managed $ bracket (TTF.blended font (V4 0 0 0 255) text) SDL.freeSurface
    texture <- managed $ bracket (SDL.createTextureFromSurface r surface) SDL.destroyTexture
    (w, h) <- TTF.size font text
    let rect = Just $ SDL.Rectangle (fromIntegral <$> A.P p) (fromIntegral <$> V2 w h)
    SDL.copy r texture Nothing rect
  interpret r (is ())
