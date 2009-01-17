module Frag.Adapter.SDL 
    ( viewDrawings )
where

import Frag.Event
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.SDL as SDL
import Control.Monad (forever)

viewDrawings :: Event (Draw.Draw ()) -> IO ()
viewDrawings event = do
    SDL.init [SDL.InitVideo]
    SDL.setVideoMode 800 600 32 [SDL.OpenGL]
    forever $ do
        d <- waitEvent event
        Draw.draw d
        SDL.glSwapBuffers
