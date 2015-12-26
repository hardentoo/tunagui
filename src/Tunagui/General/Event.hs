module Tunagui.General.Event
  ( listenAllEvents
  ) where

import           Control.Concurrent    (forkIO)
import           Control.Monad         (unless, when)
import           FRP.Sodium
import qualified Linear.Affine         as A

import qualified SDL

import           Tunagui.General.Data  (FrameEvents (..))
import qualified Tunagui.General.Types as T

type EventPusher = SDL.Event -> Reactive ()

listenAllEvents :: IO FrameEvents
listenAllEvents = do
  (ps, events) <- sync $ do
    (eQuit', pQuit) <- quitEvent
    (ePML', pPML) <- mouseEvent SDL.Pressed  SDL.ButtonLeft
    (eRML', pRML) <- mouseEvent SDL.Released SDL.ButtonLeft
    --
    behQuit' <- hold False eQuit'
    --
    let ps = [pQuit, pPML, pRML]
        events = FrameEvents
          { behQuit = behQuit'
          , ePML = ePML'
          , eRML = eRML'
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

-- | TODO: Each window should have quit event
quitEvent :: Reactive (EventPair Bool)
quitEvent = fmap work <$> newEvent
  where
    work push e = when (SDL.eventPayload e == SDL.QuitEvent) $ push True

mouseEvent :: SDL.InputMotion -> SDL.MouseButton -> Reactive (EventPair (SDL.Window, T.Point Int))
mouseEvent motion button = fmap work <$> newEvent
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
