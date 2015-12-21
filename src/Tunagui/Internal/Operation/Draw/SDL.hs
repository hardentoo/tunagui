{-# LANGUAGE GADTs #-}

module Tunagui.Internal.Operation.Draw.SDL
  (
    runDraw
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
import           Tunagui.General.Data            (TWindow (..))
import           Tunagui.Internal.Operation.Draw

runDraw = interpret

interpret :: TWindow -> DrawP IO a -> IO ()
interpret t is = eval t =<< viewT is

eval :: TWindow -> ProgramViewT DrawI IO a -> IO ()
eval _ (Return _) = return ()
eval tw i = eval' i
  where
    rnd = twRenderer tw
    convP (T.P p) = A.P $ fromIntegral <$> p
    convS (T.S s) = fromIntegral <$> s
    --
    eval' (Clear :>>= is) = SDL.clear rnd >> interpret tw (is ())
    eval' (Flush :>>= is) = SDL.present rnd >> interpret tw (is ())
    eval' (SetColor c :>>= is) = do
      SDL.rendererDrawColor rnd $= c
      interpret tw (is ())
    eval' (FillRect p s :>>= is) = do
      SDL.fillRect rnd $ Just (SDL.Rectangle (convP p) (convS s))
      interpret tw (is ())
    eval' (DrawRect p s :>>= is) = do
      SDL.drawRect rnd $ Just (SDL.Rectangle (convP p) (convS s))
      interpret tw (is ())
