module Frag.Reactive 
    ( Reactive
    , switch
    , step
    , destruct 
    )
where

import Prelude hiding (until)
import Frag.Event
import Control.Comonad
import Control.Applicative
import Control.Monad (MonadPlus(..))

data Reactive a = Reactive a (Event (Reactive a))

instance Functor Reactive where
    fmap f (Reactive x xs) = Reactive (f x) ((fmap.fmap) f xs)

instance Applicative Reactive where
    pure x = Reactive x mzero
    fr@(Reactive f fs) <*> xr@(Reactive x xs) =
        Reactive (f x) (fmap (<*> xr) fs `mplus` fmap (fr <*>) xs)

instance Copointed Reactive where
    extract (Reactive x _) = x

instance Comonad Reactive where
    duplicate r@(Reactive x xs) = Reactive r (fmap duplicate xs)


switch :: a -> Event (Reactive a) -> Reactive a
switch = Reactive

step :: a -> Event a -> Reactive a
step x xs = Reactive x (fmap (\x' -> step x' xs) xs)

destruct :: Reactive a -> (a, Event (Reactive a))
destruct (Reactive x xs) = (x,xs)
