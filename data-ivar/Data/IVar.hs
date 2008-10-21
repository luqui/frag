----------------------------------------
-- |
-- Module    : Data.IVar
-- Copyright : (c) Luke Palmer 2008
-- License   : BSD3
--
-- Maintainer  : Luke Palmer <lrpalmer@gmail.com>
-- Stability   : experimental
-- Portability : presumably portable
--
-- An IVar is an write-once variable (the name comes from \"immutable
-- variable\").  In addition to encapsulating this common idiom,
-- it also provides a way to block on multiple variables simultaneously,
-- resuming when the first of them is written.
--
-- This module is careful not to create memory leaks, and prefers to
-- maintain good long-term memory performance than to be super-fast.
-- It should be reasonably fast though.
--
-- This module is designed to be imported qualified, as in:
--
-- > import qualified Data.IVar as IVar
--
-- Example:  
--
-- > import qualified Data.IVar as IVar
-- > import Control.Concurrent
-- > 
-- > main = do
-- >    iv <- IVar.new
-- >    iv' <- IVar.new
-- >    forkIO $ threadDelay 10000000 >> IVar.write iv' "my spoon is too big"
-- >    let merger = IVar.read iv `mplus` IVar.read iv'
-- >    print =<< IVar.nonblocking merger   -- most likely "Nothing"
-- >    print =<< IVar.blocking merger      -- waits a while, then prints
-- >    IVar.write iv' "i am a banana"      -- throws error "IVar written twice"

module Data.IVar 
    ( IVar, new, write, read
    , Reader, nonblocking, blocking, combo
    )
where

import Prelude hiding (read)
import Control.Concurrent.MVar
import Control.Applicative
import Data.IORef
import Data.Monoid
import Control.Monad
import Data.Unique
import Control.Exception
import qualified Data.Map as Map

data State a
    = Value a                               -- a value
    | NoValue (Map.Map Unique (a -> IO ())) -- or a set of blockers

-- | A write-once variable.
newtype IVar a = IVar (IORef (State a))

-- | Create a new empty IVar.
new :: IO (IVar a)
new = IVar <$> newIORef (NoValue Map.empty)

-- | Write a value to an IVar.  If the IVar already has a value, 
-- throws an error \"Attempt to write to an IVar twice\".
write :: IVar a -> a -> IO ()
write (IVar ref) x = block $ do
    b <- atomicModifyIORef ref $ \v ->
        case v of
            Value a -> (v, Nothing)
            NoValue blockers -> (Value x, Just blockers)
    case b of
        Nothing -> fail "Attempt to write to an IVar twice"
        Just blockers -> do
            mapM_ ($ x) (Map.elems blockers)

-- | Read an IVar into the 'Reader' functor. Pass this to
-- 'blocking' or 'nonblocking' to extract the value.
read :: IVar a -> Reader a
read var@(IVar ref) = Reader $ do
    state <- readIORef ref
    return $ case state of
        Value x -> Left x
        NoValue _ -> Right [LogEntry var (\x -> Reader (return (Left x)))]

-- Actions will be called in a blocked state; they must unblock themselves.
-- This is because it is impossible to achieve the exception safety of
-- and action t'other way round.

addAction :: IVar a -> (a -> IO ()) -> IO Unique
addAction (IVar ref) action = do
    actionid <- newUnique
    block . join . atomicModifyIORef ref $ \v -> 
        case v of
            Value x -> (v, action x)
            NoValue blockers -> (NoValue (Map.insert actionid action blockers), return ())
    return actionid

deleteAction :: IVar a -> Unique -> IO ()
deleteAction (IVar ref) actionid = do
    atomicModifyIORef ref $ \v ->
        case v of
            Value x -> (v, ())
            NoValue blockers -> 
                -- This is strict so that we *actually* clean up
                -- after ourselves; otherwise ivars that get waited
                -- on but never filled could cause memory leaks
                let m = Map.delete actionid blockers
                in m `seq` (NoValue m, ())

-- Reader is a free monad over (lists of) this functor; i.e. 
-- it either returns a value or says "here is the variable 
-- I blocked on, and here's what I'd do if I got it."
data LogEntry a = forall r. LogEntry (IVar r) (r -> a)

instance Functor LogEntry where
    fmap f (LogEntry v cc) = LogEntry v (f . cc)

-- | Reader is a functor (also monad) for reading IVars.  This
-- provides composability when blocking on the first of a set
-- of IVars, as you can block on several IVars of different
-- types.
--
-- The MonadPlus and Monoid instances for Reader are equivalent.
-- It tries the left action ; if it blocks, then it tries the
-- right action ; if /it/ blocks, then the whole action blocks
-- until one of the two is available.
newtype Reader a = Reader { runReader :: IO (Either a [LogEntry (Reader a)]) }

f +++ g = either (Left . f) (Right . g)

instance Functor Reader where
    fmap f (Reader m) = Reader (fmap (f +++ (fmap.fmap.fmap) f) m)

instance Monad Reader where
    return = Reader . return . Left
    m >>= f = Reader $ do
        r <- runReader m
        case r of
            Left x -> runReader $ f x
            Right log -> return $ Right ((fmap.fmap) (>>= f) log)

instance Applicative Reader where
    pure = return
    (<*>) = ap


instance Monoid (Reader a) where
    mempty = Reader . return . Right $ []
    mappend m m' = Reader $ do
        a <- runReader m
        case a of
          Left x -> return (Left x)
          Right log -> do
            b <- runReader m'
            case b of
              Left y -> return (Left y)
              Right log' -> do
                return (Right (log ++ log'))

instance MonadPlus Reader where
    mzero = mempty
    mplus = mappend


-- | Run a reader nonblocking.  Returns @Just x@ if a value @x@ is
-- available, @Nothing@ otherwise.
nonblocking :: Reader a -> IO (Maybe a)
nonblocking reader = do
    r <- runReader reader
    return $ case r of
        Left x -> Just x
        Right _ -> Nothing

-- | Block on a reader.  Returns the value as soon as it is
-- available.
blocking :: Reader a -> IO a
blocking reader = do
    r <- runReader reader
    case r of
        Left x -> return x
        Right log -> primBlocking log

primBlocking :: [LogEntry (Reader a)] -> IO a
primBlocking log = do
    blocker <- newEmptyMVar
    cleanup <- block . forM log $ \(LogEntry var action) -> do
        ident <- addAction var (\v -> tryPutMVar blocker (action v) >> return ())
        return $ deleteAction var ident
    blocking =<< takeMVar blocker

-- | Combination nonblocking and blocking read.  @combo r@ 
-- Returns @Left x@ if the value is available now, otherwise 
-- returns @Right (blocking r)@.  This is more efficient than
-- using nonblocking and blocking in sequence (it only evaluates
-- the Reader once).
combo :: Reader a -> IO (Either a (IO a))
combo reader = do
    r <- runReader reader
    case r of
        Left x -> return (Left x)
        Right log -> return . Right $ primBlocking log
