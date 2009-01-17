import Graphics.DrawingCombinators as Draw
import Frag.Adapter.SDL
import Frag.Event
import Frag.Time
import qualified Graphics.UI.SDL as SDL
import Control.Concurrent
import Control.Monad (forM_)
import Data.Monoid (Monoid(..))

data Builder
    = Empty
    | Cont Builder Builder Builder

builderSteps :: Builder -> [Builder]
builderSteps Empty = repeat Empty
builderSteps (Cont b1 b2 b3) = 
    Empty : zipWith3 Cont (builderSteps b1) (builderSteps b2) (builderSteps b3)


triangle :: Draw.Draw ()
triangle = mconcat [ Draw.line p1 p2, Draw.line p2 p3, Draw.line p3 p1 ]
    where
    p1 = (0,1)
    p2 = (-x,y)
    p3 = (x,y)
    x = sin (2*pi/3)
    y = cos (2*pi/3)

drawBuilder :: Builder -> Draw.Draw ()
drawBuilder Empty = Draw.empty
drawBuilder (Cont left right bottom) = mconcat [
    triangle,
    Draw.rotate (-pi/3) . Draw.translate (0,1) . drawBuilder $ left,
    Draw.rotate (pi/3)  . Draw.translate (0,1) . drawBuilder $ right,
    Draw.rotate pi      . Draw.translate (0,1) . drawBuilder $ bottom ]

evolveB :: Builder -> TimeFun (Event (Draw.Draw ()))
evolveB b = buildEvent . forM_ (builderSteps b) $ \builder -> do
    fire . drawBuilder $ builder
    delay 0.5

testBuilder = let r = Cont r r Empty in r

main = do
    drawings <- runTimeFun (evolveB testBuilder)
    viewDrawings (fmap (Draw.scale (1/10) (1/10)) drawings)
