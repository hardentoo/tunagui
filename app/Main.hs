{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent     (threadDelay)
import           Control.Monad.IO.Class (liftIO)
import           FRP.Sodium
import           Linear.V2

import qualified Tunagui                as GUI
import           Tunagui                (runTuna, WidgetTree (..), Direction (..)
                                        ,withWindow, WinConfig (..))
import           Tunagui.Widget         (ButtonConfig (..), defaultButtonConfig
                                        ,defaultLabelConfig
                                        ,onClick)
import           Tunagui.Operation

main :: IO ()
main =
  GUI.withTunagui $ \tuna ->
    -- 1st window
    withWindow (WinConfig "main" True (V2 600 400)) tuna $ \win1 -> do
      _ <- runTuna tuna $ runTWin win1 $ do
        (btn1, w1B) <- mkButton (defaultButtonConfig {bcText = Just "button1"})
        (_, w1L) <- mkLabel defaultLabelConfig "Label"
        testOverwriteTreeOP (Container DirV [w1B,w1L])
        testRenderTree
        liftIO . sync $ listen (onClick btn1) $ \p -> putStrLn $ "click (1): " ++ show p
      -- -- 2nd window
      withWindow (WinConfig "sub" False (V2 200 200)) tuna $ \win2 -> do
        _ <- runTuna tuna $ runTWin win2 $ do
          (btn2, w2) <- mkButton (defaultButtonConfig {bcText = Just "button2"})
          testOverwriteTreeOP (Container DirV [w2])
          testRenderTree
          liftIO . sync $ listen (onClick btn2) $ \p -> putStrLn $ "click (2): " ++ show p
        --
        let loop = do
              putStrLn "."
              threadDelay 1000000
              -- q <- sync $ sample quit
              -- unless q loop
              loop
        liftIO loop
