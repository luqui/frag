import Frag.Event
import Frag.Reactive
import Frag.SDL
import qualified Graphics.DrawingCombinators as Draw
import Control.Applicative
import qualified Graphics.UI.SDL as SDL

main = sdlMain $ \e -> 
    let mouse = filterEvent mouseFilter e
        pos = step (0,0) mouse
    in
    Draw.translate <$> pos <*> pure Draw.circle

filterEvent :: (a -> Maybe b) -> Event a -> Event b
filterEvent f e = maybe (filterEvent f e) return =<< f <$> e

mouseFilter :: SDL.Event -> Maybe (Double,Double)
mouseFilter (SDL.MouseMotion x y _ _) = 
    Just (fromIntegral x / 320 - 1, 1 - fromIntegral y / 240)
mouseFilter _ = Nothing
