module Frag.Behavior where

import Prelude hiding (until)
import Frag.Time
import Frag.Kernel
import Frag.RWChan
import Control.Concurrent.MVar
import qualified Data.QuotientRef as QRef
import Control.Applicative
import Data.Maybe
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad

-- The monad in which we do all our computation, Eval.
-- It is just a writer of Requests, where the monoid action
-- on requests is to merge rather than append.

newtype RequestM = RequestM [Request]

instance Monoid RequestM where
    mempty = RequestM []
    mappend (RequestM xs) (RequestM ys) = RequestM (mergeRequests xs ys)

-- Semantically, Eval is the identity monad.  In the implementation,
-- it is responsible for taking all the requests made in a computation
-- and making sure they get to the top so they can be satisfied.
data Eval a = Eval RequestM a

instance Functor Eval where
    fmap f (Eval rs x) = Eval rs (f x)

instance Monad Eval where
    return x = Eval mempty x
    -- Can we get away with making this lazier?
    Eval rs a >>= f = let Eval rs' b = f a in Eval (rs `mappend` rs') b


-- Semantically, ComputationIDs are just unit, they mean nothing.  But
-- in the implementation, they separate different sets of incoming
-- monotone times.  Essentially they are a table in which to cache
-- behavior IDs (actually, because of the way QRef works, it's more
-- like an inverse table; the IDs themselves are the tables,
-- and the compid is an index).
newtype ComputationID = ComputationID (forall a. QRef.RightRef a)

-- Behavior, what we've been working for!  Semantically a function of time.
-- In this implementation, it takes a computation ID to cache itself,
-- a list of times at which to evaluate, and returns the list of values
-- evaluated there.  
newtype Behavior a 
    = Behavior { runBehavior :: ComputationID -> Eval ([Time] -> Eval [a]) }

-- Synchronize a computation with a lock.
synch :: MVar () -> IO a -> IO a
synch lock action = do
    takeMVar lock
    r <- action
    putMVar lock ()
    return r

type PrimBehavior a = [Time] -> Eval [a]
    

makeBehavior :: forall a. (ComputationID -> [Time] -> Eval [a]) -> Behavior a
-- this first unsafe is to allocate a unique ID to the lexical behavior (the QRef)
makeBehavior f = unsafePerformIO $ do  
    ref <- QRef.newLeft
    lock <- newMVar ()
    return (behavior ref lock)

    where

    behavior :: QRef.LeftRef (PrimBehavior a) -> MVar () -> Behavior a
    -- this second unsafe is to try to retrieve the behavior from the cache
    -- and allocate new channels if not.
    behavior ref lock = Behavior $ \(ComputationID compid) -> unsafePerformIO $ do
        synch lock $ do
            maybecache <- QRef.read ref compid
            case maybecache of
                Just x -> return (return x)
                Nothing -> do
                    Eval reqs prim <- createEvalFunc (ComputationID compid)
                    QRef.write ref compid prim
                    return (Eval reqs prim)
    
    createEvalFunc :: ComputationID -> IO (Eval (PrimBehavior a))
    createEvalFunc compid = do
        (times, timechan) <- newWChan
        let Eval reqs as = f compid times
        valchan <- newRChan as
        return $ Eval reqs (timeFunc (writeWChan timechan) (fromJust <$> readRChan valchan))

    timeFunc :: Writer Time -> Reader a -> [Time] -> Eval [a]
    -- This third and final unsafe allocates a channel on which to 
    -- recieve the specific list of times given.
    timeFunc timechan valchan times = unsafePerformIO $ do
        (results, resultchan) <- newWChan
        let requests = for times $ makeRequest
                                     timechan valchan (writeWChan resultchan)
        return $ Eval (RequestM requests) results
    
    for = flip map


-- Yow, that is some *unsafe* code!
-- But it's all pure from here on out.

instance Functor Behavior where
    -- LOL
    fmap f (Behavior b) = Behavior (fmap (fmap (fmap (fmap (fmap f)))) b)

instance Applicative Behavior where
    pure x = Behavior (\_ -> return (\times -> return (map (const x) times)))
    f <*> x = Behavior $ \compid -> do
        f' <- runBehavior f compid
        x' <- runBehavior x compid
        return $ liftA2 (liftM2 (zipWith ($))) f' x'

-- Okay, it's not the prettiest thing in the world,
-- but you have no idea how hard I have worked to make this
-- instance even feasible! w00t!!!
instance Monad Behavior where
    return = pure
    m >>= f = Behavior $ \compid -> do
        m' <- runBehavior m compid
        return $ \times -> do
            as <- m' times
            fmap concat . forM (zip as times) $ \(a,t) -> do
                mb' <- runBehavior (f a) compid
                mb' [t]


until :: Behavior a -> Future (Behavior a) -> Behavior a
until b fut = makeBehavior go
    where
    go compid times = do
        let (pres,posts) = span (<= time fut) times
        b' <- runBehavior b compid
        prevals <- b' pres
        f' <- runBehavior (value fut) compid
        postvals <- f' posts
        return $ prevals ++ postvals
