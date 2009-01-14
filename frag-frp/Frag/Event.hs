module Frag.Event 
    ( Time, shiftTime, diffTime
    , Event
    , merge, never, withTime, filterMap, filter, once
    , EventBuilder, wait, currentTime, fire
    
    -- legacy adapters
    , Sink, buildEvent, newEventSink, waitEvent
    )
where

import Prelude hiding (filter)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Time.Clock
import Data.Time.Calendar
import Control.Arrow (second)
import Data.Monoid (Monoid(..))
import Control.Monad.Reader
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)

newtype Time = Time UTCTime
    deriving (Eq, Ord)

shiftTime :: Double -> Time -> Time
shiftTime amt (Time t) = Time $ addUTCTime (realToFrac amt) t

diffTime :: Time -> Time -> Double
diffTime (Time t) (Time t') = realToFrac (diffUTCTime t t')

-- | > Event a = Set (Time, a)
newtype Event a = Event { runEvent :: UTCTime -> STM (UTCTime, a) }

instance Functor Event where
    fmap f (Event e) = Event ((fmap.fmap.second) f e)

-- | > merge = union -- (where union is left-biased)
merge :: Event a -> Event a -> Event a
merge e e' = Event $ \t -> do
    let e1 = fmap Left (runEvent e t)
        e2 = fmap Right (runEvent e' t)
    r <- liftM2 (,) (e1 `orElse` e2) (e2 `orElse` e1)
    return $ case r of
        (Left x , Left _)  -> x
        (Right y, Right _) -> y
        (Left x , Right y) -> least x y
        (Right y, Left x)  -> least x y
    where
    least (t,x) (t',x') | t <= t'   = (t,x)
                        | otherwise = (t',x') 

-- | > never = {}
never :: Event a
never = Event . const $ retry

instance Monoid (Event a) where
    mempty = never
    mappend = merge

-- | > withTime e = { (t, { (t,x) | x <- s} | (t,s) <- e }
withTime :: Event a -> Event (Time,a)
withTime (Event e) = Event ((fmap.fmap) mod e)
    where
    mod (t,x) = (t,(Time t,x))

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

unsafeStepFun :: UTCTime -> TVar Bool
{-# NOINLINE unsafeStepFun #-}
unsafeStepFun t = unsafePerformIO $ do
    now <- getCurrentTime
    if t < now
        then newTVarIO True
        else do
            var <- newTVarIO False
            forkIO $ do
                threadDelay . ceiling . (1000000*) . realToFrac $ diffUTCTime t now
                atomically $ writeTVar var True
            return var

-- | > once t x = { (t,x) }
once :: Time -> a -> Event a
once (Time t) x = Event $ \t' -> do
    if t < t' then retry else do
        let var = unsafeStepFun t
        v <- readTVar var
        unless v retry
        return (t, x)

type Sink a = a -> IO ()

-- | > EventBuilder r a = WriterT (Event r) (State Time) a
--                      = Time -> (Time, Event r, a)
newtype EventBuilder r a = EventBuilder { runEventBuilder :: ReaderT (Sink r) IO a }

-- | > wait e t = (t', never, x)
--   >     where (t',x) = least occurrence of e >= t
wait :: Event a -> EventBuilder r a
wait e = EventBuilder . lift . waitEvent $ e

-- | > currentTime = (t, never, t)
currentTime :: EventBuilder r Time
currentTime = EventBuilder . lift . fmap Time $ getCurrentTime

-- | > fire x t = (t, once t x, ())
fire :: r -> EventBuilder r ()
fire v = EventBuilder $ do
    sink <- ask
    lift $ sink v

buildEvent :: EventBuilder r () -> IO (Event r)
buildEvent builder = do
    (event, sink) <- newEventSink
    forkIO $ runReaderT (runEventBuilder builder) sink
    return event

negativeInfinity = UTCTime (ModifiedJulianDay 0) (fromIntegral 0)

newEventSink :: IO (Event a, a -> IO ())
newEventSink = do
    var <- atomically $ newTVar (negativeInfinity, error "this never happened")
    let event = Event $ \t -> do
            (t', x) <- readTVar var
            if t' < t then retry else return (t', x)
        sink val = do
            t <- getCurrentTime
            atomically $ writeTVar var (t,val)
    
    return (event, sink)

waitEvent :: Event a -> IO a
waitEvent e = do
    now <- getCurrentTime
    (t,x) <- atomically $ runEvent e now
    return x
