{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (liftIO)
import           FRP.Sodium
import           Linear.V2
import qualified Data.Text              as T

import qualified Tunagui                as GUI
import           Tunagui                (runTuna, WidgetTree (..), Direction (..)
                                        ,withWindow, WinConfig (..))
import           Tunagui.Widget         (onClick)
import qualified Tunagui.Widget.Button  as Button
import qualified Tunagui.Widget.Label   as Label
import           Tunagui.Operation

main :: IO ()
main = select

select :: IO ()
select = GUI.withTunagui $ \tuna ->
  withWindow (WinConfig "Select" False (V2 300 300)) tuna $ \win -> do
    runTuna tuna $ runWin win $ do
      (btnButton, wRunButton) <- Button.new $ Button.defaultConfig {Button.bcText = Just "Button test"}
      (btnLabel, wRunLabel) <- Button.new $ Button.defaultConfig {Button.bcText = Just "Label test"}
      --
      testOverwriteTreeOP $ Container DirV [wRunButton, wRunLabel]
      testRenderTree
      --
      liftIO $ btnButton `onClick` testButton tuna
      liftIO $ btnLabel `onClick` testLabel tuna
    forever $ do -- TODO: Clear thread leak
      putStrLn "."
      threadDelay 1000000

testButton :: GUI.Tunagui -> IO ()
testButton tuna =
  withWindow (WinConfig "main" True (V2 300 300)) tuna $ \win -> do
    runTuna tuna $ runWin win $ do
      ws1 <- Container DirV <$> mapM mkBtn [1..(5::Int)]
      ws2 <- Container DirH <$> mapM mkBtn [6..(10::Int)]
      testOverwriteTreeOP $ Container DirV [ws1,ws2]
      testRenderTree
      --
    forever $ do -- Clear thread leak
      putStrLn "."
      threadDelay 1000000
  where
    mkBtn i = do
      (btn,w) <- Button.new $ Button.defaultConfig {Button.bcText = text}
      liftIO $ btn `onClick` work
      return w
      where
        text = Just $ T.pack $ show i
        work = putStrLn $ "click: " ++ show i

testLabel :: GUI.Tunagui -> IO ()
testLabel tuna =
  withWindow (WinConfig "main" True (V2 300 300)) tuna $ \win -> do
    (beh, push) <- liftIO . sync $ newBehavior (0 :: Integer)
    _ <- runTuna tuna $ runWin win $ do
      (btnP,wBtnP) <- Button.new (Button.defaultConfig {Button.bcText = Just "+"})
      (btnM,wBtnM) <- Button.new (Button.defaultConfig {Button.bcText = Just "-"})
      (btnC,wBtnC) <- Button.new (Button.defaultConfig {Button.bcText = Just "CLEAR"})
      (_,wLbl) <- Label.newB Label.defaultConfig (T.pack . show <$> beh)
      let cBtn = Container DirH [wBtnP, wBtnM]
      testOverwriteTreeOP (Container DirV [cBtn,wLbl,wBtnC])
      testRenderTree
      --
      liftIO $ do
        btnP `onClick` sync (push . (+ 1) =<< sample beh)
        btnM `onClick` sync (push . (+ (-1)) =<< sample beh)
        btnC `onClick` sync (push 0)

    liftIO . forever $ do -- Clear thread leak
      putStrLn "."
      threadDelay 1000000
