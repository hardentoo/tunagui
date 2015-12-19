module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)

import qualified Tunagui            as GUI

main :: IO ()
main =
  GUI.withTunagui GUI.Settings $ \_ ->
    forever $ do
      putStrLn "."
      threadDelay 1000000
