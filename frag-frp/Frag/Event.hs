module Frag.Event 
    ( Event
    , merge, never, withTime, filterMap, filter
    -- Legacy adapters
    , newEventSink, waitEvent
    )
where

import Prelude hiding (filter)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Time.Clock
import Control.Arrow (second)
import Data.Monoid (Monoid(..))

newtype Time = Time UTCTime
    deriving (Eq, Ord)

newtype Event a = Event { runEvent :: UTCTime -> STM (UTCTime, a) }

instance Functor Event where
    fmap f (Event e) = Event ((fmap.fmap.second) f e)

merge :: Event a -> Event a
merge e e' = Event $ \t -> do
    let e1 = fmap Left (runEvent e t)
        e2 = fmap Right (runEvent e' t)
    r <- liftM2 (,) (e1 `orElse` e2) (e2 `orElse` e1)
    return $ case r of
        (Left x , Left _)  = x
        (Right y, Right _) = y
        (Left x , Right y) = least x y
        (Right y, Left x)  = least x y
    where
    least (t,x) (t',x') | t <= t'   = (t,x)
                        | otherwise = (t',x') 

never :: Event a
never = Event . const $ retry

instance Monoid (Event a) where
    mempty = never
    mappend = merge


withTime :: Event a -> Event (Time,a)
withTime (Event e) = Event ((fmap.fmap) mod e)
    where
    mod (t,x) = (t,(Time t,x))

filterMap :: (a -> Maybe b) -> Event a -> Event b
filterMap p e = Event $ \t -> do
    (t',x) <- runEvent e t
    case p x of
        Nothing -> retry
        Just y -> return (t',y)

filter :: (a -> Bool) -> Event a -> Event a
filter p = filterMap (\x -> if p x then Just x else Nothing)



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
