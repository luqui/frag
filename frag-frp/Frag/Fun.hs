module Frag.Behavior where

import Frag.Reactive
import Control.Applicative hiding (Const(Const))

data Fun a b
    = Const b
    | Fun (a -> b)

evalFun :: Fun a b -> a -> b
evalFun (Const b) _ = b
evalFun (Fun f) x   = f x

instance Functor (Fun a) where
    fmap f (Const x) = Const (f x)
    fmap f (Fun t) = Fun (f . t)

instance Applicative (Fun a) where
    pure = Const
    Const f <*> Const x  = Const (f x)
    Const f <*> Fun xf   = Fun (\t -> f      (xf t))
    Fun ff  <*> Const x  = Fun (\t -> (ff t) x)
    Fun ff  <*> Fun xf   = Fun (\t -> (ff t) (xf t))

instance Monad (Fun a) where
    return = Const
    Const b >>= f = f b
    Fun ff  >>= f = Fun (\t -> evalFun (f (ff t)) t)
