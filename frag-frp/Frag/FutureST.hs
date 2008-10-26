module Frag.FutureST where

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
