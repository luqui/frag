module Frag.NatTrie 
    ( NatTrie, uniform, operate, lookup, modify, insert) 
where

import Control.Applicative
import Prelude hiding (lookup)
import Data.Monoid

data NatTrie a
    = NatTrie a (NatTrie a) (NatTrie a)

instance Functor NatTrie where
    fmap f (NatTrie x l r) = NatTrie (f x) (fmap f l) (fmap f r)

instance Applicative NatTrie where
    pure = uniform
    NatTrie f fl fr <*> NatTrie x xl xr 
        = NatTrie (f x) (fl <*> xl) (fr <*> xr)

instance (Monoid m) => Monoid (NatTrie m) where
    mempty = pure mempty
    mappend = liftA2 mappend

uniform :: a -> NatTrie a
uniform x = let r = NatTrie x r r in r

operate :: Integer -> (a -> a) -> NatTrie a -> (a, NatTrie a)
operate 0 f ~(NatTrie x l r) = let fx = f x in fx `seq` (x, NatTrie fx l r)
-- I think this strictness is necessary for GC,    ^^^   but it's something to play with.
operate n f ~(NatTrie x l r)
    | rem == 0   = let (x', l') = operate quot f l in (x', NatTrie x l' r)
    | otherwise  = let (x', r') = operate quot f r in (x', NatTrie x l r')
    where
    (quot,rem) = quotRem n 2

lookup :: Integer -> NatTrie a -> a
lookup n = fst . operate n id

modify :: Integer -> (a -> a) -> NatTrie a -> NatTrie a
modify n f = snd . operate n f

insert :: Integer -> a -> NatTrie a -> NatTrie a
insert n = modify n . const



