module Frag.Knot ( Knot, write, extract, runKnot ) where

import MonadLib
import Data.Monoid

newtype Knot w a = Knot (WriterT w (StateT w Id) a)
    deriving (Functor,Monad)

write :: (Monoid w) => w -> Knot w ()
write = Knot . put

extract :: (Monoid w) => (w -> (a,w)) -> Knot w a
extract f = Knot $ do
    w <- get
    let (x,w') = f w
    set w'
    return x

runKnot :: Knot w a -> a
runKnot (Knot m) = let ((x,_),w) = runId (runStateT w (runWriterT m)) in x
                       --     ^---------------------^
                       --          tying the knot
