module Main where

import qualified Tunagui as GUI

main :: IO ()
main =
  GUI.withTunagui GUI.Settings $ \tng -> putStrLn "work"
