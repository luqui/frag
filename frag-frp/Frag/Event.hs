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

newtype PrimMap a = PrimMap (Map.Map Unique (Any -> a))

instance Functor PrimMap where
    fmap f (PrimMap m) = PrimMap ((fmap.fmap) f m)

-- | @Event a@ is conceptually an event that can occur.
--
-- Semantics: @Event a = Time -> (Time,a)@; where the
-- returned time is no less than the argument.  Intuitively,
-- \"wait some amount of time and yield a value\".
--
-- Functor, Monad, Applicative are all the same as State.
data Event a
    = Return a
    | Wait [PrimMap (Event a)]

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

-- | An event that never occurs. 
--
-- Semantics: @never t = (infinity, undefined)@
never :: Event a
never = Wait []

-- | The first of two events to occur.
--
-- Semantics: 
--
-- > firstOf e e' t0 = minBy (comparing fst) (e t0) (e' t0)
-- >     where
-- >     minBy f x y = case f x y of
-- >                       LT -> x
-- >                       EQ -> x
-- >                       GT -> y

firstOf :: Event a -> Event a -> Event a
firstOf (Return x) _ = Return x
firstOf _ (Return x) = Return x
firstOf (Wait m) (Wait m') = Wait (m ++ m')
    

-- Event is not an instance of MonadPlus, because
-- mplus does not distribute over >>=.
instance Monoid (Event a) where
    mempty = never
    mappend = firstOf


newtype Dispatcher = Dispatcher (Chan (Unique, Any))

newDispatcher :: IO Dispatcher
newDispatcher = Dispatcher <$> newChan

newEventTrigger :: Dispatcher -> IO (Event a, a -> IO ())
newEventTrigger (Dispatcher chan) = do
    ident <- newUnique
    let event   = Wait [PrimMap $ Map.singleton ident (Return . unsafeCoerce)]
        trigger x = writeChan chan (ident, unsafeCoerce x)
    return (event, trigger)

evolvePrim :: Unique -> Any -> PrimMap (Event a) -> Event a
evolvePrim ident val (PrimMap m) = 
    case Map.lookup ident m of
        Nothing -> Wait [PrimMap m]
        Just cc -> cc val

waitEvent :: Dispatcher -> Event a -> IO a
waitEvent _ (Return x) = return x
waitEvent (Dispatcher chan) (Wait cs) = do
    (ident, val) <- readChan chan
    waitEvent (Dispatcher chan) . foldr1 firstOf . map (evolvePrim ident val) $ cs
