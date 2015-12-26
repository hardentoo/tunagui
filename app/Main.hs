module Main where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (unless)
import           Control.Monad.IO.Class     (liftIO)
import           FRP.Sodium

import qualified Tunagui                    as GUI
import           Tunagui.Operation
import           Tunagui.Widget.Features    (onClick)
import           Tunagui.Widget.Prim.Button

-- test
import Tunagui.Widget.Layout
import Tunagui.General.Data

main :: IO ()
main =
  GUI.withTunagui GUI.Settings $ do
    -- testOperation
    (btn1, w1) <- mkButton $ ButtonConfig 50 20
    (btn2, w2) <- mkButton $ ButtonConfig 40 20
    (btn3, w3) <- mkButton $ ButtonConfig 100 100
    _ <- liftIO . sync . listen (onClick btn1) $ \p -> putStrLn $ "click (1): " ++ show p
    _ <- liftIO . sync . listen (onClick btn2) $ \p -> putStrLn $ "click (2): " ++ show p
    _ <- liftIO . sync . listen (onClick btn3) $ \p -> putStrLn $ "click (3): " ++ show p
    quit <- quitBehav
    --
    -- liftIO $ do
      -- withTWindow (Container DirV [w1, w2]) $ \tw1 ->
        -- withTWindow (Container DirV [w3]) $ \tw2 -> do
    tw1 <- liftIO . newTWindow $ Container DirV [w1, w2]
    testRenderTree tw1
    --
    tw2 <- liftIO . newTWindow $ Container DirV [w3]
    testRenderTree tw2
    --
    let loop = do
          putStrLn "."
          threadDelay 1000000
          q <- sync $ sample quit
          unless q loop
    liftIO loop
    -- --
    liftIO $ freeTWindow tw1
    liftIO $ freeTWindow tw2
