module Frag.Event 
    ( Event, future, computation, runEvent )
where

import Control.Monad.ST.Lazy
import Frag.Time
import Control.Applicative
import Control.Monad

-- Event is a "future computation"

data FVal r a = Exact a | Next (ST r (Event r a))
newtype Event r a = Event { unEvent :: Future (FVal r a) }

instance Functor (FVal r) where
    fmap f (Exact x) = Exact (f x)
    fmap f (Next s)  = Next ((fmap.fmap) f s)

instance Functor (Event r) where
    fmap f (Event s) = Event ((fmap.fmap) f s)

instance Monad (Event r) where
    return = Event . return . Exact
    Event m >>= f = Event (join (fmap bind m))
        where
        bind (Exact x) = unEvent (f x)
        bind (Next st) = return (Next (fmap (>>= f) st))

instance Applicative (Event r) where
    pure = return
    (<*>) = ap

future :: Future a -> Event r a
future = Event . fmap Exact

computation :: Future (ST r (Event r a)) -> Event r a
computation = Event . fmap Next

commute :: Future (ST r a) -> ST r (Future a)
commute fut = fmap (makeFuture (time fut)) (value fut)

runEvent :: Event r a -> ST r (Future a)
runEvent (Event m) = fmap join (commute (fmap go m))
    where
    go (Exact a) = return (return a)
    go (Next st) = runEvent =<< st
