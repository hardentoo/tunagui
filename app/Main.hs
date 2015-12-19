module Main where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)

import qualified Tunagui as GUI

main :: IO ()
main =
  GUI.withTunagui GUI.Settings $ \_ ->
    forever $ do
      putStrLn "."
      threadDelay 1000000
