{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tunagui.Internal.Base
  ( Base, runBase
  , liftIO
  ) where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State

import           Tunagui.General.Data (TunaContents (..), TunaState (..))

newtype Base a = Base {
    runB :: ReaderT TunaContents (StateT TunaState IO) a
  } deriving (Functor, Applicative,
              Monad, MonadIO,
              MonadReader TunaContents, MonadState TunaState)

runBase :: Base a -> TunaContents -> TunaState -> IO (a, TunaState)
runBase k contents state =
  runStateT (runReaderT (runB k) contents) state
