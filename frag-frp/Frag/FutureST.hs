module Frag.FutureST 
    ( FutureST, future, computation, runFutureST )
where

import Control.Monad.ST.Lazy
import Frag.Time
import Control.Applicative
import Control.Monad

-- FutureC is a "future computation"

data FVal r a = Exact a | Next (ST r (FutureST r a))
newtype FutureST r a = FutureST { unFutureST :: Future (FVal r a) }

instance Functor (FVal r) where
    fmap f (Exact x) = Exact (f x)
    fmap f (Next s)  = Next ((fmap.fmap) f s)

instance Functor (FutureST r) where
    fmap f (FutureST s) = FutureST ((fmap.fmap) f s)

instance Monad (FutureST r) where
    return = FutureST . return . Exact
    FutureST m >>= f = FutureST (join (fmap bind m))
        where
        bind (Exact x) = unFutureST (f x)
        bind (Next st) = return (Next (fmap (>>= f) st))

instance Applicative (FutureST r) where
    pure = return
    (<*>) = ap

future :: Future a -> FutureST r a
future = FutureST . fmap Exact

computation :: Future (ST r (FutureST r a)) -> FutureST r a
computation = FutureST . fmap Next

commute :: Future (ST r a) -> ST r (Future a)
commute fut = fmap (makeFuture (time fut)) (value fut)

runFutureST :: FutureST r a -> ST r (Future a)
runFutureST (FutureST m) = fmap join (commute (fmap go m))
    where
    go (Exact a) = return (return a)
    go (Next st) = runFutureST =<< st
