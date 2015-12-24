{-# LANGUAGE ExistentialQuantification #-}

module Tunagui.Widget.Layout where

import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Tunagui.Internal.Operation.Render (RenderP)
import           Tunagui.Widget.Features           (Renderable, render)

data WidgetTree =
  forall a. (Show a, Renderable a)
  => Widget a | Container Direction [WidgetTree]

data Direction
  = DirH -- Horizontal
  | DirV -- Vertical
  deriving Show

instance Show WidgetTree where
  show (Widget a) = "Widget " ++ show a
  show (Container dir ws) = "Container " ++ show dir ++ " " ++ show ws

-- TODO: This is test code. Fix it.
pushW :: (Show a, Renderable a) => a -> WidgetTree -> WidgetTree
pushW a (Container dir ws) = Container dir (ws ++ [Widget a])
pushW _ (Widget _) = error "Undefined! Change this code!"

renderWT :: MonadIO m => WidgetTree -> RenderP m ()
renderWT (Widget a)       = do
  liftIO $ print a
  render a
renderWT (Container _ ws) = do
  liftIO $ print ws
  mapM_ renderWT ws
