module Frag.Future where

import Control.Monad
import Frag.PrimFuture

data Future a where
    Return  :: a -> Future a
    Suspend :: [PrimFuture (Future a)] -> Future a

instance Functor Future where
    fmap f (Return x) = Return (f x)
    fmap f (Suspend cs) = Suspend ((fmap.fmap.fmap) f cs)


instance Monad Future where
    return = Return
    Return x >>= f = f x
    Suspend cs >>= f = Suspend ((fmap.fmap) (>>= f) cs)

instance MonadPlus Future where
    mzero = Suspend []

    Return x   `mplus` _           = Return x
    Suspend _  `mplus` Return x    = Return x
    Suspend cs `mplus` Suspend cs' = Suspend (cs ++ cs')


