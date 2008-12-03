module Frag.Future 
    ( Future
    , primFutureToFuture
    , waitFuture, waitMultipleFutures
    )
where

import Control.Monad
import Frag.PrimFuture
import Control.Applicative

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

instance Applicative Future where
    pure = return
    (<*>) = ap

primFutureToFuture :: PrimFuture a -> Future a
primFutureToFuture pf = Suspend [fmap Return pf]

waitFuture :: Listener -> Future a -> IO a
waitFuture listener f = fmap head $ waitMultipleFutures listener [f]

waitMultipleFutures :: Listener -> [Future a] -> IO [a]
waitMultipleFutures listener fs = do
    let immediate = [ a | Return a <- fs ]
    let pfs = [ pf | Suspend pfs' <- fs, pf <- pfs' ]
    case immediate of
        [] -> waitMultipleFutures listener =<< waitFutures listener pfs
        _  -> return immediate
