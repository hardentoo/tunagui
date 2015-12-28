{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent     (threadDelay)
import           Control.Monad.IO.Class (liftIO)
import           FRP.Sodium
import           Linear.V2

import qualified Tunagui                as GUI
import           Tunagui                (runTuna, WidgetTree (..), Direction (..)
                                        ,withTWindow, WinConfig (..))
import           Tunagui.Widget         (ButtonConfig (..), defaultButtonConfig
                                        ,onClick)
import           Tunagui.Operation

main :: IO ()
main =
  GUI.withTunagui $ \tuna ->
    -- 1st window
    withTWindow (WinConfig "main" True (V2 600 400)) tuna $ \tw1 -> do
      _ <- runTuna tuna $ runTWin tw1 $ do
        (btn1, w1) <- mkButton (defaultButtonConfig
          { btnMinWidth = Just 100
          , btnMinHeight = Just 50
          , bcText = Just "button1"
          })
        testOverwriteTreeOP (Container DirV [w1])
        testRenderTree
        liftIO . sync $ listen (onClick btn1) $ \p -> putStrLn $ "click (1): " ++ show p
      -- -- 2nd window
      withTWindow (WinConfig "sub" False (V2 200 200)) tuna $ \tw2 -> do
        _ <- runTuna tuna $ runTWin tw2 $ do
          (btn2, w2) <- mkButton (defaultButtonConfig
            { btnMinWidth = Just 50
            , btnMinHeight = Just 100
            , bcText = Just "button2"
            })
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
