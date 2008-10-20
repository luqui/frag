import Control.Monad.State
import Control.Monad.Writer

newtype Simple b c a = Simple (StateT [c] (Writer [b]) a)
    deriving (Functor, Monad)

runSimple :: ([b] -> [c]) -> Simple b c a -> a
runSimple f (Simple m) = 
    let (x,bs) = runWriter (evalStateT m (f bs)) in x

call :: b -> Simple b c c
call b = Simple $ do
    tell [b]
    ~(x:xs) <- get
    modify tail
    return x
