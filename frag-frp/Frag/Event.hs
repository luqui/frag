{-# LANGUAGE RecursiveDo, GeneralizedNewtypeDeriving #-}

module Frag.Event 
    ( Event
    , merge, mergeWith, never, withTime, filterMap, filter, once
    , EventBuilder, wait, delay, liftTime, fire
    , buildEvent
    
    -- legacy adapters
    , Sink, newEventSink, waitEvent
    )
where

import Prelude hiding (filter)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Arrow (second)
import Data.Monoid (Monoid(..))
import Control.Monad.Reader
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (ap)
import Control.Applicative
import System.Mem.Weak

import Frag.Time

-- | > Event a = Set (Time, a)
-- @(never, merge)@ forms a Monoid on @Event a@.
newtype Event a = Event { runEvent :: Time -> STM (Time, a) }

instance Functor Event where
    fmap f (Event e) = Event ((fmap.fmap.second) f e)

-- | > merge = mergeWith const
-- @mappend@ on the Monoid instance
merge = mergeWith const

-- | > mergeWith = unionWith
mergeWith :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeWith mergefun e e' = Event $ \t -> do
    let e1 = fmap Left (runEvent e t)
        e2 = fmap Right (runEvent e' t)
    r <- liftM2 (,) (e1 `orElse` e2) (e2 `orElse` e1)
    return $ case r of
        (Left x , Left _)  -> x
        (Right y, Right _) -> y
        (Left x , Right y) -> least x y
        (Right y, Left x)  -> least x y
    where
    least (t,x) (t',x') =
        case compare t t' of
            LT -> (t, x)
            EQ -> (t, mergefun x x')
            GT -> (t', x')

-- | > never = {}
-- @mempty@ on the Monoid instance.
never :: Event a
never = Event . const $ retry

instance Monoid (Event a) where
    mempty = never
    mappend = merge

-- | > withTime e = { (t, { (t,x) | x <- s} | (t,s) <- e }
withTime :: Event a -> Event (Time,a)
withTime (Event e) = Event ((fmap.fmap) modify e)
    where
    modify (t,x) = (t,(t,x))

-- | > filterMap p e = { (t,y) | (t,x) <- e, Just y <- p x }
filterMap :: (a -> Maybe b) -> Event a -> Event b
filterMap p e = Event $ \t -> do
    (t',x) <- runEvent e t
    case p x of
        Nothing -> retry
        Just y -> return (t',y)

-- | > filter p = filterMap (\x -> if p x then Just x else Nothing)
filter :: (a -> Bool) -> Event a -> Event a
filter p = filterMap (\x -> if p x then Just x else Nothing)

unsafeStepFun :: Time -> TVar Bool
{-# NOINLINE unsafeStepFun #-}
unsafeStepFun t = unsafePerformIO $ do
    now <- runTimeFun time
    if t < now
        then newTVarIO True
        else do
            var <- newTVarIO False
            forkIO $ do
                threadDelay . ceiling . (1000000*) $ diffTime t now
                atomically $ writeTVar var True
            return var

-- | > once t x = { (t,x) }
once :: Time -> a -> Event a
once t x = Event $ \t' -> do
    if t < t' then retry else do
        let var = unsafeStepFun t
        v <- readTVar var
        unless v retry
        return (t, x)

type Sink a = a -> IO ()

-- | > EventBuilder r a = WriterT (Event r) (State Time) a
--                      = Time -> (Time, Event r, a)
newtype EventBuilder r a = EventBuilder { runEventBuilder :: ReaderT (Sink r) IO a }
    deriving (Functor,Monad)

instance Applicative (EventBuilder r) where
    pure = return
    (<*>) = ap

-- | > liftTime f t = (t, never, f t)
liftTime :: TimeFun a -> EventBuilder r a
liftTime = EventBuilder . lift . runTimeFun

-- | > wait e t = (t', never, x)
--   >     where (t',x) = least occurrence of e after t
wait :: Event a -> EventBuilder r a
wait e = EventBuilder . lift . waitEvent $ e

-- | > delay dt t = (t+dt, never, ())
delay :: Double -> EventBuilder r ()
delay t = EventBuilder . lift . threadDelay . ceiling $ 1000000*t

-- | > fire x t = (t, once t x, ())
-- (currently this does not respect the semantics, in that multiple events
-- fired at the same time will take the last one, rather than the first
-- as specified by the merge semantics)
fire :: r -> EventBuilder r ()
fire v = EventBuilder $ do
    sink <- ask
    lift $ sink v

-- 'Thread blocked indefinitely' in reasonable situations with this impl.
-- I wonder what's going on.
{-
buildEvent :: EventBuilder r () -> TimeFun (Event r)
buildEvent builder = unsafeIOToTimeFun $ mdo
    var <- atomically $ newTVar (negativeInfinity, error "this never happened")
    wvar <- mkWeakPtr var (Just (killThread threadid))
    let event = Event $ \t -> do
            (t', x) <- readTVar var
            if t' <= t then retry else return (t',x)
        sink val = do
            var' <- deRefWeak wvar
            case var' of
                Nothing -> return ()
                Just v -> do
                    t <- runTimeFun time
                    atomically $ writeTVar v (t,val)
    threadid <- forkIO $ runReaderT (runEventBuilder builder) sink
    forkIO $ runReaderT (runEventBuilder builder) sink
    return event
-}

-- | > buildEvent b t = let (_, e, _) = b t in e
buildEvent :: EventBuilder r () -> TimeFun (Event r)
buildEvent builder = unsafeIOToTimeFun $ do
    (event, sink) <- newEventSink
    forkIO $ runReaderT (runEventBuilder builder) sink
    return event


newEventSink :: IO (Event a, a -> IO ())
newEventSink = do
    var <- atomically $ newTVar (negativeInfinity, error "this never happened")
    let event = Event $ \t -> do
            (t', x) <- readTVar var
            if t' <= t then retry else return (t', x)
        sink val = do
            t <- runTimeFun time
            atomically $ writeTVar var (t,val)
    
    return (event, sink)

waitEvent :: Event a -> IO a
waitEvent e = do
    now <- runTimeFun time
    (_,x) <- atomically $ runEvent e now
    return x
