{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tunagui.General.Base
  ( TunaguiT
  , runTuna
  ) where

import           Control.Monad.Reader
import           Control.Monad.State

import           Tunagui.General.Data (Tunagui)

newtype TunaguiT a = TunaguiT {
    runT :: ReaderT Tunagui IO a
  } deriving (Functor, Applicative,
              Monad, MonadIO,
              MonadReader Tunagui)

runTuna :: Tunagui -> TunaguiT a -> IO a
runTuna tuna k = runReaderT (runT k) tuna
