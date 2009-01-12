module Frag.SDL (sdlMain) where

import qualified Graphics.UI.SDL as SDL
import Frag.Event
import Frag.Reactive
import qualified Graphics.DrawingCombinators as Draw
import Control.Monad (forever)
import Control.Concurrent (forkIO)

sdlMain :: (Event SDL.Event -> Reactive (Draw.Draw ())) -> IO ()
sdlMain f = do
    SDL.init [SDL.InitVideo]
    SDL.setVideoMode 640 480 32 [SDL.OpenGL]
    disp <- newDispatcher
    (event, sink) <- newEventTrigger disp
    forkIO $ eventLoop sink
    mainLoop disp (f event)

mainLoop :: Dispatcher -> Reactive (Draw.Draw ()) -> IO ()
mainLoop disp r = do
    let (x,xs) = destruct r
    Draw.draw x
    SDL.glSwapBuffers
    mainLoop disp =<< waitEvent disp xs

eventLoop :: (SDL.Event -> IO ()) -> IO ()
eventLoop sink = forever $ sink =<< SDL.waitEvent
