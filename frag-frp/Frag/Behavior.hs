module Frag.Behavior
    ( Sink, Behavior, liftEnv, until, runBehaviorSamples )
where

import Prelude hiding (until)
import Control.Applicative
import Control.Concurrent.STM
import Data.Monoid (Monoid(..))

import Frag.Env
import Frag.Event

-- | An idempotent sink @s@ has the property that for every @x@,
-- @s x >> s x = s x@.
type Sink a = a -> IO ()

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

liftEnv :: Env a -> Behavior a
liftEnv env = Behavior env mempty

until :: Behavior a -> Event b -> (b -> Env (Behavior a)) -> Behavior a
until (Behavior env ev) ev' trans = Behavior env (ltrans `mappend` rtrans)
    where
    ltrans = fmap (\b' -> until b' ev' trans) ev
    rtrans = joinEventEnv $ fmap trans ev'

runBehaviorSamples :: Double -> Sink a -> Behavior a -> IO ()
runBehaviorSamples rate sink b@(Behavior env cont) = do
    -- if it's a DynEnv, then timeout in 'rate' seconds
    -- otherwise never timeout (i.e. block until next event)
    timeout <- case internalProjectEnv env of
                    Left x   -> return retry
                    Right io -> do
                        delayvar <- registerDelay (rateToMicrosecs rate)
                        return $ assert (return b) =<< readTVar delayvar
     
    sink =<< runEnv env
    stm <- internalRunEvent cont
    next <- atomically $ stm `orElse` timeout
    runBehaviorSamples rate sink =<< runEnv next
    where
    rateToMicrosecs r = floor (1000000/r)
    assert x p = if p then return x else retry
