module Main where

import           Control.Concurrent         (threadDelay)
import           Control.Monad.IO.Class     (liftIO)
import           FRP.Sodium

import qualified Tunagui                    as GUI
import           Tunagui                    (WidgetTree (..), Direction (..))
import           Tunagui.Operation
import           Tunagui.Widget.Features    (onClick)
import           Tunagui.Widget.Prim.Button

-- test
import Tunagui.General.Data

main :: IO ()
main =
  GUI.withTunagui GUI.Settings $ \(Tunagui cnt _) ->
    -- 1st window
    withTWindow (cntEvents cnt) $ \tw1 -> do
      _ <- runTWin tw1 $ do
        (btn1, w1) <- mkButton (ButtonConfig 100 30)
        testOverwriteTreeOP (Container DirV [w1])
        testRenderTree
        liftIO . sync $ listen (onClick btn1) $ \p -> putStrLn $ "click (1): " ++ show p
      -- 2nd window
      withTWindow (cntEvents cnt) $ \tw2 -> do
        _ <- runTWin tw2 $ do
          (btn2, w2) <- mkButton (ButtonConfig 30 100)
          testOverwriteTreeOP (Container DirV [w2])
          testRenderTree
          liftIO . sync $ listen (onClick btn2) $ \p -> putStrLn $ "click (2): " ++ show p
        --
        let loop = do
              putStrLn "."
              threadDelay 1000000
              -- q <- sync $ sample quit
              -- unless q loop
              loop
        liftIO loop
