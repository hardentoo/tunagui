{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tunagui.General.Tunagui
  ( Tunagui, runTunagui
  ) where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State

import           Tunagui.General.Data (TunaContents (..), TunaState (..))

newtype Tunagui a = Tng {
    runTng :: ReaderT TunaContents (StateT TunaState IO) a
  } deriving (Functor, Applicative,
              Monad, MonadIO,
              MonadReader TunaContents, MonadState TunaState)

runTunagui :: TunaContents -> Tunagui a -> IO (a, TunaState)
runTunagui contents k =
  runStateT (runReaderT (runTng k) contents) state
  where
    state = TunaState 0
