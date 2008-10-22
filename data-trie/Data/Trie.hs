module Data.Trie 
    ( Trie(..)
    , NatTrie
    )
where

import Prelude hiding (lookup)

class (Functor t) => Trie t k | t -> k where
    uniform :: a -> t a
    modify  :: (a -> a) -> k -> t a -> t a
    lookup  :: k -> t a -> a
    union   :: (a -> a -> a) -> t a -> t a -> t a


data NatTrie v
    = NatTrie { ntVal :: v, ntFalse :: NatTrie v, ntTrue :: NatTrie v }

instance Functor NatTrie where
    fmap f (NatTrie x l r) = NatTrie (f x) (fmap f l) (fmap f r)

instance Trie NatTrie Integer where
    uniform x = let r = NatTrie x r r in r
    modify f = go . integralBits
        where
        go []         ~(NatTrie x l r) = NatTrie (f x) l r
        go (False:xs) ~(NatTrie x l r) = NatTrie x (go xs l) r
        go (True:xs)  ~(NatTrie x l r) = NatTrie x l (go xs r)
    lookup = go . integralBits
        where
        go []         = ntVal
        go (False:xs) = go xs . ntFalse
        go (True:xs)  = go xs . ntTrue
    union f (NatTrie x l r) (NatTrie x' l' r') = 
        NatTrie (f x x') (union f l l') (union f r r')


integralBits :: (Integral a) => a -> [Bool]
integralBits 0 = []
integralBits x = let (q,r) = quotRem x 2 in toBool r : integralBits q
    where
    toBool 0 = False
    toBool 1 = True
