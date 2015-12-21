module Main where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (liftIO)
-- import           Control.Monad.State    (get)


import qualified Tunagui                as GUI

import qualified Tunagui.Operation      as OP

main :: IO ()
main =
  GUI.withTunagui GUI.Settings $ do
    OP.testOperation
    liftIO . forever $ do
      putStrLn "."
      threadDelay 1000000
