module Frag.Reactive where

import Prelude hiding (until)
import Frag.Future
import Control.Applicative
import Control.Monad
import Control.Comonad

data Reactive a = Reactive a (Future (Reactive a))

instance Functor Reactive where
    fmap f (Reactive x xs) = Reactive (f x) ((fmap.fmap) f xs)

instance Applicative Reactive where
    pure x = Reactive x mzero 
    -- an optimization opportunity here,  if we can determine sometimes that a
    -- Future will never occur (unsafeIsNever or something)
    f@(Reactive f' fs) <*> x@(Reactive x' xs) =
        Reactive (f' x') (fmap (<*> x) fs `mplus` fmap (f <*>) xs)


instance Copointed Reactive where
    extract (Reactive x _) = x

instance Comonad Reactive where
    duplicate r@(Reactive x xs) = Reactive r (fmap duplicate xs)


until :: Reactive a -> Future (Reactive a) -> Reactive a
until (Reactive x xs) fut = Reactive x (xs `mplus` fut)

snapshot :: Reactive a -> Future b -> Future (a,b)
snapshot (Reactive x fxs) fut = 
    (fxs >>= \xs -> snapshot xs fut) `mplus` fmap ((,) x) fut

