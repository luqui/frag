module Frag.Event 
    ( Event, joinEventEnv, snapshot, makeEvent, internalRunEvent )
where

import Control.Concurrent.STM
import Control.Monad (ap, MonadPlus(..), join)
import Control.Applicative
import Data.Monoid (Monoid(..))
import Frag.Env

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

joinEventEnv :: Event (Env a) -> Event a
joinEventEnv (Event m) = Event ((fmap.fmap) join m)

snapshot :: Env a -> Event b -> Event (a,b)
snapshot env event = Event $ fmap (liftA2 (,) env) <$> runEvent event

makeEvent :: IO (Event a, a -> IO ())
makeEvent = do
    chan <- atomically newTChan
    let sink = atomically . writeTChan chan
    let event = Event $ do
            chan' <- atomically $ dupTChan chan
            return . fmap return $ readTChan chan'
    return (event, sink)

internalRunEvent :: Event a -> IO (STM (Env a))
internalRunEvent = runEvent
