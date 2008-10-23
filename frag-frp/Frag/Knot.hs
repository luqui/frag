module Frag.Knot where

import Data.Monoid hiding (Any)
import qualified Frag.NatTrie as Trie
import GHC.Prim (Any)
import Unsafe.Coerce
import Control.Monad
import Control.Applicative
import Data.Maybe

newtype ID a b = ID Integer

type Satisfier = Trie.NatTrie (Maybe Any)

data Knot :: * -> * where
    Return  :: Satisfier -> a -> Knot a
    Request :: ID b c -> b -> (c -> Knot a) -> Knot a

mergeTries :: Satisfier -> Satisfier -> Satisfier 
mergeTries = liftA2 mplus

addTo :: Satisfier -> Knot a -> Knot a
addTo sat (Return sat' x) = Return (mergeTries sat sat') x
addTo sat (Request req inp cont) = Request req inp (addTo sat . cont)

instance Functor Knot where
    fmap f (Return t a) = Return t (f a)
    fmap f (Request id b cont) = Request id b (fmap f . cont)

instance Monad Knot where
    return = Return (Trie.uniform Nothing)
    Return trie a >>= f = addTo trie (f a)
    Request req inp cont >>= f = Request req inp (cont >=> f)

runKnot' :: Knot a -> (Satisfier, a)
runKnot' (Return sat x) = (sat,x)
runKnot' (Request (ID req) inp cont) = 
    let (sat,x)           = runKnot' (cont outp)
        (sat',(outp,kf')) = runKnot' (runProcessor (unsafeCoerce (fromJust (Trie.lookup req sat''))) inp)
        sat''             = sat -- XXX must be wrong!
    in (Trie.insert req (Just (unsafeCoerce kf')) sat'', x)

runKnot :: Knot a -> a
runKnot = snd . runKnot' 

newtype Processor a b = Processor { runProcessor :: a -> Knot (b, Processor a b) }

nonlocal :: Integer -> Processor a b -> Knot (a -> Knot b)
nonlocal ident proc = Return sat (\b -> Request (ID ident) b return)
    where
    sat = Trie.insert ident (Just (unsafeCoerce proc)) (Trie.uniform Nothing)
