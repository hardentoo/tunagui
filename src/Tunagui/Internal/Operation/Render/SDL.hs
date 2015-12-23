{-# LANGUAGE GADTs #-}

module Tunagui.Internal.Operation.Render.SDL
  (
    runRender
  ----- Operation
  -- Basic
  , clear
  , flush
  , setColor
  -- Rect
  , fillRect
  , drawRect
  ) where

import           Control.Monad.Operational
import           Linear.V2
import qualified Linear.Affine as A

import           SDL                             (($=))
import qualified SDL

import qualified Tunagui.General.Types as T
import           Tunagui.Internal.Operation.Render

runRender :: SDL.Renderer -> RenderP IO a -> IO ()
runRender = interpret

interpret :: SDL.Renderer -> RenderP IO a -> IO ()
interpret r is = eval r =<< viewT is

convP :: (Integral a, Integral b) => T.Point a -> A.Point V2 b
convP (T.P p) = A.P $ fromIntegral <$> p

convS :: (Integral a, Integral b) => T.Size a -> V2 b
convS (T.S s) = fromIntegral <$> s

eval :: SDL.Renderer -> ProgramViewT RenderI IO a -> IO ()
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
