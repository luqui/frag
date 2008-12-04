module Frag.Future 
    ( Future
    , fromPrim
    , wait, waitMultiple
    )
where

import Control.Monad
import qualified Frag.PrimFuture as PrimFuture
import Control.Applicative

data Future a where
    Return  :: a -> Future a
    Suspend :: [PrimFuture.PrimFuture (Future a)] -> Future a

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

fromPrim :: PrimFuture.PrimFuture a -> Future a
fromPrim pf = Suspend [fmap Return pf]

wait :: PrimFuture.Listener -> Future a -> IO a
wait listener f = fmap head $ waitMultiple listener [f]

waitMultiple :: PrimFuture.Listener -> [Future a] -> IO [a]
waitMultiple listener fs = do
    let immediate = [ a | Return a <- fs ]
    let pfs = [ pf | Suspend pfs' <- fs, pf <- pfs' ]
    case immediate of
        [] -> waitMultiple listener =<< PrimFuture.wait listener pfs
        _  -> return immediate
