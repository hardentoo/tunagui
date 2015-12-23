module Main where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (liftIO)

import           FRP.Sodium

import qualified Tunagui                as GUI

import           Tunagui.Operation

main :: IO ()
main =
  GUI.withTunagui GUI.Settings $ do
    testOperation
    btn <- mkButton
    e <- onClick btn
    _ <- liftIO . sync . listen e $ \p -> putStrLn $ "click: " ++ show p
    --
    liftIO . forever $ do
      putStrLn "."
      threadDelay 1000000
