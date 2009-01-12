module Frag.StateMachine
    ( StateMachine
    , switch
    , step
    , destruct 
    )
where

import Frag.Event
import Control.Comonad
import Control.Applicative
import Control.Monad (MonadPlus(..))

data StateMachine a = StateMachine a (Event (StateMachine a))

instance Functor StateMachine where
    fmap f (StateMachine x xs) = StateMachine (f x) ((fmap.fmap) f xs)

instance Applicative StateMachine where
    pure x = StateMachine x mzero
    fr@(StateMachine f fs) <*> xr@(StateMachine x xs) =
        StateMachine (f x) (fmap (<*> xr) fs `mplus` fmap (fr <*>) xs)

instance Copointed StateMachine where
    extract (StateMachine x _) = x

instance Comonad StateMachine where
    duplicate r@(StateMachine x xs) = StateMachine r (fmap duplicate xs)


switch :: a -> Event (StateMachine a) -> StateMachine a
switch = StateMachine

step :: a -> Event a -> StateMachine a
step x xs = StateMachine x (fmap (\x' -> step x' xs) xs)

destruct :: StateMachine a -> (a, Event (StateMachine a))
destruct (StateMachine x xs) = (x,xs)
