{-# LANGUAGE ExistentialQuantification #-}

module Tunagui.Widget.Layout where

import           Control.Monad                     (foldM, void)
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Data.Foldable                     (foldl')
import           FRP.Sodium
import           Linear.V2

import qualified Tunagui.General.Types             as T
import           Tunagui.Internal.Operation.Render (RenderP)
import           Tunagui.Widget.Features           (Renderable, locate, render)

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
pushW _ (Widget _) = error "Undefined! Change this code! @Layout"

locateWT :: WidgetTree -> IO ()
locateWT widgetTree = void . sync $ go widgetTree (T.P (V2 0 0))
  where
    go :: WidgetTree -> T.Point Int -> Reactive (T.Range Int)
    go (Widget a)         p0 = locate a p0
    go (Container dir ws) p0 = do
      ranges <- foldM locate' [T.R p0 p0] ws
      return $ T.R (foldl' leftTop p0 ranges) (foldl' rightBottom p0 ranges)
      where
        locate' :: [T.Range Int] -> WidgetTree -> Reactive [T.Range Int]
        locate' rs@(r:_) tree = (:rs) <$> work tree (nextFrom r)
          where
            work (Widget a)          = locate a
            work cnt@(Container _ _) = go cnt

        leftTop :: Ord a => T.Point a -> T.Range a -> T.Point a
        leftTop point range =
          let (T.P (V2 x y)) = point
              (T.R (T.P (V2 x' y')) _) = range
          in T.P (V2 (min x x') (min y y'))

        rightBottom :: Ord a => T.Point a -> T.Range a -> T.Point a
        rightBottom point range =
          let (T.P (V2 x y)) = point
              (T.R _ (T.P (V2 x' y'))) = range
          in T.P (V2 (max x x') (max y y'))

        nextFrom (T.R (T.P (V2 x0 y0)) (T.P (V2 x1 y1))) =
          case dir of
            DirH -> T.P (V2 x1 y0)
            DirV -> T.P (V2 x0 y1)

renderWT :: MonadIO m => WidgetTree -> RenderP m ()
renderWT (Widget a)       = render a
renderWT (Container _ ws) = mapM_ renderWT ws
