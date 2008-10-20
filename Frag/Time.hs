module Frag.Time where

import qualified Data.IVar as IVar
import Data.Time.Clock
import Control.Applicative

data Time
    = Always
    | Exact (IVar.Reader UTCTime)
    | Never

data Future a = Future Time a

order :: Future a -> Future a -> (Future a, Future a)
order f@(Future t x) f'@(Future t' x') = (lesser, greater)
    where
    (lesser,greater) = 
        case (t,t') of
            (Always,_) -> (f,f')
            (_,Always) -> (f',f)
            (_,Never)  -> (f,f')
            (Never,_)  -> (f',f)
            (Exact r, Exact r') -> unsafePerformIO . IVar.blocking $ do
                a <- (Left <$> r) `mappend` (Right <$> r')
                b <- (Right <$> r') `mappend` (Left <$> r)
                case (a,b) of
                    (Left x, Left _) -> (f,f')
                    (Right y, Right _) -> (f',f)
                    (Left x, Right y) -> merge x y
                    (Right y, Left x) -> merge x y
    merge t t' | t <= t'   = (f,f')
               | otherwise = (f',f)
