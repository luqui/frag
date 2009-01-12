module Frag.Event 
    ( Event
    , Dispatcher, newDispatcher, newEventTrigger, waitEvent
    )
where

import Data.Unique
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad (MonadPlus(..), ap)
import Control.Applicative
import qualified Data.Map as Map
import Control.Concurrent.Chan
import Data.Monoid (Monoid(..))

data Event a
    = Return a
    | Wait (Map.Map Unique (Any -> Event a))

instance Functor Event where
    fmap f (Return x) = Return (f x)
    fmap f (Wait cs)  = Wait ((fmap.fmap.fmap) f cs)

instance Monad Event where
    return = Return
    
    Return x >>= f = f x
    Wait cs >>= f = Wait ((fmap.fmap) (>>= f) cs)

instance Applicative Event where
    pure = return
    (<*>) = ap

instance MonadPlus Event where
    mzero = Wait Map.empty
    Return x `mplus` _ = Return x
    _ `mplus` Return x = Return x
    Wait cs `mplus` Wait cs' = Wait (Map.unionWith (liftA2 mplus) cs cs')

instance Monoid (Event a) where
    mempty = mzero
    mappend = mplus


newtype Dispatcher = Dispatcher (Chan (Unique, Any))

newDispatcher :: IO Dispatcher
newDispatcher = Dispatcher <$> newChan

newEventTrigger :: Dispatcher -> IO (Event a, a -> IO ())
newEventTrigger (Dispatcher chan) = do
    ident <- newUnique
    let event   = Wait (Map.singleton ident (Return . unsafeCoerce))
        trigger = writeChan chan . unsafeCoerce
    return (event, trigger)

waitEvent :: Dispatcher -> Event a -> IO a
waitEvent _ (Return x) = return x
waitEvent (Dispatcher chan) (Wait cs) = go
    where
    go = do
        (ident, val) <- readChan chan
        case Map.lookup ident cs of
            Nothing -> go
            Just f -> waitEvent (Dispatcher chan) (f val)
