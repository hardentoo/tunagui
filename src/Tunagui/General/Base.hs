{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tunagui.General.Base
  (
    Tunagui (..)
  , FrameEvents (..)
  , TunaguiT, runTuna
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           FRP.Sodium

import qualified SDL
import qualified SDL.Font              as TTF

import qualified Tunagui.General.Types as T

data Tunagui = Tunagui
  { cntEvents  :: FrameEvents
  , cntFont :: TTF.Font
  }

data FrameEvents = FrameEvents
  { behQuit :: Behavior Bool
  , eWinClosed :: Event SDL.Window
  , ePML  :: Event (SDL.Window, T.Point Int) -- Press Mouse Left
  , eRML  :: Event (SDL.Window, T.Point Int) -- Release Mouse Left
  }

newtype TunaguiT a = TunaguiT {
    runT :: ReaderT Tunagui IO a
  } deriving (Functor, Applicative,
              Monad, MonadIO,
              MonadReader Tunagui)

runTuna :: Tunagui -> TunaguiT a -> IO a
runTuna tuna k = runReaderT (runT k) tuna
