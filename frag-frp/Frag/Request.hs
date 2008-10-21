module Frag.Request where

import Prelude hiding (lookup)
import Frag.Time
import Data.Unique
import Data.Monoid
import Control.Applicative
import Control.Monad (ap)
import Unsafe.Coerce

newtype ReqID a = ReqID Unique

newReqID :: IO (ReqID a)
newReqID = ReqID <$> newUnique

compareReqID :: ReqID a -> ReqID b -> Maybe (Equal a b)
compareReqID (ReqID x) (ReqID y) 
    | x == y = Just (unsafeCoerce Refl)
    | otherwise = Nothing

data Equal :: * -> * -> * where
    Refl :: Equal a a

data ReqEntry :: * -> *  where
    ReqEntry :: Time -> ReqID b -> (b -> r) -> ReqEntry r

data Request :: * -> * -> * where
    Return  :: d -> a -> Request d a
    Request :: ReqEntry (Request d a) -> Request d a

class (Monoid d) => RequestMap d where
    lookup    :: ReqEntry (Request d a) -> Request d a

instance Functor ReqEntry where
    fmap f (ReqEntry t id r) = ReqEntry t id (f . r)

instance Functor (Request d) where
    fmap f (Return handler x) = Return handler (f x)
    fmap f (Request req) = Request (fmap (fmap f) req)

instance (RequestMap d) => Monad (Request d) where
    return = Return mempty
    Return handler a >>= f = 
        case f a of
            Return handler' b -> Return (handler `mappend` handler') b
            Request req -> lookup req
    Request req >>= f = Request (fmap (>>= f) req)

instance (RequestMap d) => Applicative (Request d) where
    pure = return
    (<*>) = ap
