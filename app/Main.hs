module Main where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forever)
import           Control.Monad.IO.Class     (liftIO)
import           FRP.Sodium

import qualified Tunagui                    as GUI
import           Tunagui.Operation
import           Tunagui.Widget.Features    (onClick)
import           Tunagui.Widget.Prim.Button

main :: IO ()
main =
  GUI.withTunagui GUI.Settings $ do
    testOperation
    btn1 <- mkButton $ ButtonConfig 50 20
    btn2 <- mkButton $ ButtonConfig 40 20
    _ <- liftIO . sync . listen (onClick btn1) $ \p -> putStrLn $ "click (1): " ++ show p
    _ <- liftIO . sync . listen (onClick btn2) $ \p -> putStrLn $ "click (2): " ++ show p
    mapM_ pushWidget [btn1, btn2]
    testRenderTree
    --
    liftIO . forever $ do
      putStrLn "."
      threadDelay 1000000
