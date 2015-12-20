module Main where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (get)

import qualified Tunagui                as GUI

main :: IO ()
main =
  GUI.withTunagui GUI.Settings $ do
    s <- get
    liftIO $ forever $ do
                putStrLn "."
                print s
                threadDelay 1000000
