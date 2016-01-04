module Tunagui.General.Event
  ( listenAllEvents
  ) where

import           Control.Concurrent    (forkIO)
import           Control.Monad         (unless, when)
import           FRP.Sodium
import qualified Linear.Affine         as A

import qualified SDL

import           Tunagui.General.Base  (FrameEvents (..))
import qualified Tunagui.General.Types as T

type EventPusher = SDL.Event -> Reactive ()

listenAllEvents :: IO FrameEvents
listenAllEvents = do
  (ps, events) <- sync $ do
    (eQuit', pQuit) <- mkQuitEvent
    (eWinClosed', pWinClosed) <- mkWinClosedEvent
    (ePML', pPML) <- mkMouseButtonEvent SDL.Pressed  SDL.ButtonLeft
    (eRML', pRML) <- mkMouseButtonEvent SDL.Released SDL.ButtonLeft
    (eMMPos', pMMPos) <- mkMouseMotionEvent
    --
    behQuit' <- hold False eQuit'
    let ps = [pQuit, pPML, pRML, pWinClosed, pMMPos]
        events = FrameEvents
          { behQuit = behQuit'
          , eWinClosed = eWinClosed'
          , ePML = ePML'
          , eRML = eRML'
          , eMMPos = eMMPos'
          }
    return (ps, events)
  _ <- forkIO $ eventLoop ps
  return events
  where
    eventLoop :: [EventPusher] -> IO ()
    eventLoop ps = go
      where
        go = do
          es <- SDL.pollEvents
          mapM_ sync [f e | f <- ps, e <- es]
          unless (any isQuit es) go
        --
        isQuit e = SDL.eventPayload e == SDL.QuitEvent

type EventPair a = (Event a, EventPusher)

mkQuitEvent :: Reactive (EventPair Bool)
mkQuitEvent = fmap work <$> newEvent
  where
    work push e = when (SDL.eventPayload e == SDL.QuitEvent) $ push True

mkWinClosedEvent :: Reactive (EventPair SDL.Window)
mkWinClosedEvent = fmap work <$> newEvent
  where
    work push e =
      case SDL.eventPayload e of
        SDL.WindowClosedEvent (SDL.WindowClosedEventData win) -> push win
        _ -> return ()

mkMouseButtonEvent :: SDL.InputMotion -> SDL.MouseButton -> Reactive (EventPair (SDL.Window, T.Point Int))
mkMouseButtonEvent motion button = fmap work <$> newEvent
  where
    work push e =
      case SDL.eventPayload e of
        SDL.MouseButtonEvent dat -> do
          let win = SDL.mouseButtonEventWindow dat
          let isM = SDL.mouseButtonEventMotion dat == motion
              isB = SDL.mouseButtonEventButton dat == button
              (A.P p) = SDL.mouseButtonEventPos dat
              point = fromIntegral <$> T.P p
          when (isM && isB) $ push (win, point)
        _ -> return ()

mkMouseMotionEvent :: Reactive (EventPair (SDL.Window, T.Point Int))
mkMouseMotionEvent = fmap work <$> newEvent
  where
    work push e =
      case SDL.eventPayload e of
        SDL.MouseMotionEvent dat -> do
          let win = SDL.mouseMotionEventWindow dat
              (A.P p) = SDL.mouseMotionEventPos dat
              point = fromIntegral <$> T.P p
          push (win, point)
        _ -> return ()
