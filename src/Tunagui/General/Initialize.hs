module Tunagui.General.Initialize
  ( withTunagui
  ) where

import Control.Exception

import Tunagui.General.Data (Tunagui(..), Settings)

newTunagui :: Settings -> IO Tunagui
newTunagui _stg = return Tunagui

withTunagui :: Settings -> (Tunagui -> IO a) -> IO a
withTunagui stg work =
  bracket (newTunagui stg) release work
  where
    release _tunagui = putStrLn "release Tunagui"
