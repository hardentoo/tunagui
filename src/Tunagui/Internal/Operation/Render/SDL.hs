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

import           Control.Applicative
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Operational
import           Linear                          (V4 (..))
import qualified Linear.Affine as A

import           SDL                             (($=))
import qualified SDL

import qualified Tunagui.General.Types as T
import           Tunagui.Internal.Operation.Render

runRender = interpret

interpret :: SDL.Renderer -> RenderP IO a -> IO ()
interpret r is = eval r =<< viewT is

convP (T.P p) = A.P $ fromIntegral <$> p
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
