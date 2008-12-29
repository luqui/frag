module Frag.Env
    ( Env, runEnv, internalCastEnv, internalProjectEnv )
where

import Control.Applicative

-- | @Env a@ is semantically @Time -> a@, but with no differential
-- sensitivity (i.e. it can't "look around" at any surrounding times).
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
internalCastEnv :: IO a -> Env a
internalCastEnv = DynEnv

internalProjectEnv :: Env a -> Either a (IO a)
internalProjectEnv (ConstEnv a) = Left a
internalProjectEnv (DynEnv io) = Right io
