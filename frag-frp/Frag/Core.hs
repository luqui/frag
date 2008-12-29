module Frag.Core 
    ( Sink, Env, runEnv, unsafeCastEnv
    , Event, snapshot, makeEvent
    , Behavior, until
    , runBehaviorSamples
    )
 where

import Prelude hiding (until)
import Control.Applicative
import Data.Monoid
import Control.Concurrent.STM
import GHC.Conc (registerDelay)
import Control.Monad (ap, MonadPlus(..), join)

type Sink a = a -> IO ()

data Env a
    = ConstEnv a
    | DynEnv (IO a)

runEnv :: Env a -> IO a
runEnv (ConstEnv a) = return a
runEnv (DynEnv m) = m

instance Functor Env where
    fmap f (ConstEnv x) = ConstEnv (f x)
    fmap f (DynEnv m) = DynEnv (fmap f m)

instance Applicative Env where
    pure = ConstEnv
    ConstEnv f <*> ConstEnv x = ConstEnv (f x)
    DynEnv   f <*> ConstEnv x = DynEnv (fmap ($ x) f)
    ConstEnv f <*> DynEnv   x = DynEnv (fmap f x)
    DynEnv   f <*> DynEnv   x = DynEnv (f <*> x)

instance Monad Env where
    return = ConstEnv
    ConstEnv x >>= t = t x
    DynEnv m >>= t = DynEnv $ runEnv . t =<< m

-- Invariant: must not have any observable side-effects, and must return
-- immediately.
unsafeCastEnv :: IO a -> Env a
unsafeCastEnv = DynEnv


newtype Event a = Event { runEvent :: IO (STM (Env a)) }

instance Functor Event where
    fmap f (Event m) = Event ((fmap.fmap.fmap) f m)

instance Monad Event where
    return = Event . return . return . return
    Event m >>= t = Event $ runEvent . t =<< runEnv =<< atomically =<< m

instance Applicative Event where
    pure = return
    (<*>) = ap

instance Monoid (Event a) where
    mempty = Event $ return retry
    mappend a b = Event $ do
        stma <- runEvent a
        stmb <- runEvent b
        return (stma `orElse` stmb)

instance MonadPlus Event where
    mzero = mempty
    mplus = mappend

snapshot :: Env a -> Event b -> Event (a,b)
snapshot env event = Event $ fmap (liftA2 (,) env) <$> runEvent event

makeEvent :: IO (Event a, Sink a)
makeEvent = do
    chan <- atomically newTChan
    let sink = atomically . writeTChan chan
    let event = Event $ do
            chan' <- atomically $ dupTChan chan
            return . fmap return $ readTChan chan'
    return (event, sink)


-- A, um, "behavior", which can be translated around and do about the same thing.
-- For some definition of same.  I have no idea what its semantics are, but they
-- kinda make sense.
data Behavior a
    = Behavior (Env a) (Event (Behavior a))

instance Functor Behavior where
    fmap f (Behavior e cont) = Behavior (fmap f e) ((fmap.fmap) f cont)

instance Applicative Behavior where
    pure x = Behavior (pure x) mempty
    f@(Behavior fe fcont) <*> x@(Behavior xe xcont) =
        Behavior (fe <*> xe) (ffirst `mappend` xfirst)
        where
        ffirst = fmap (<*> x) fcont
        xfirst = fmap (f <*>) xcont


joinEventEnv :: Event (Env a) -> Event a
joinEventEnv (Event m) = Event ((fmap.fmap) join m)

until :: Env a -> Event b -> (b -> Env (Behavior a)) -> Behavior a
until env event trans = Behavior env (joinEventEnv $ fmap trans event)


runBehaviorSamples :: Double -> Sink a -> Behavior a -> IO ()
runBehaviorSamples rate sink b@(Behavior env cont) = do
    delayvar <- registerDelay (rateToMicrosecs rate)
    sink =<< runEnv env
    stm <- runEvent cont
    next <- atomically $ stm `orElse` (assert (return b) =<< readTVar delayvar)
    runBehaviorSamples rate sink =<< runEnv next
    where
    rateToMicrosecs r = floor (1000000/r)
    assert x p = if p then return x else retry
