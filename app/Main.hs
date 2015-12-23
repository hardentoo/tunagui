module Main where

import           Control.Concurrent          (threadDelay)
import           Control.Monad               (forever)
import           Control.Monad.IO.Class      (liftIO)

-- import           FRP.Sodium

import qualified Tunagui                     as GUI

import           Tunagui.Operation

import           Tunagui.Widgets.Features    (onClick)
import           Tunagui.Widgets.Prim.Button

main :: IO ()
main =
  GUI.withTunagui GUI.Settings $ do
    testOperation
    btn <- mkButton $ ButtonConfig 100 40
    -- _ <- liftIO . sync . listen (onClick btn) $ \p -> putStrLn $ "click: " ++ show p
    pushWidget btn
    testRenderTree
    --
    liftIO . forever $ do
      putStrLn "."
      threadDelay 1000000
