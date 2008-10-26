module Frag.Time 
    ( Time, KnownTime(..), occurrence, waitFor, exact, addTime
    , Future(time, value), makeFuture, order, order'
    , sinkFuture, waitFuture
    )
where

import qualified Data.IVar as IVar
import Data.Time.Clock
import Control.Applicative
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)
import Data.Monoid (Monoid(..))
import Control.Monad (ap, MonadPlus(..), forever, join)
import Control.Concurrent (threadDelay)

-- | Represents a real-world, acutal time (in an arbitrary fixed inertial
-- reference frame), extended with endpoints at positive and negative infinity.
-- Times may be indeterminate in the future, thus comparing times may block
-- until enough information is known to give a correct response.
data Time
    = Always
    | Exact (IVar.Reader UTCTime)
    | Never

-- | Represents a time in the past.  Obviously positive infinity is not included.
data KnownTime
    = NegativeInfinity
    | Known UTCTime

-- Exact has the law that the reader is stable (its value never changes once it
-- becomes defined), and the time extracted by the reader must never be before
-- the reader becomes defined.

exact :: UTCTime -> Time
exact = Exact . return

addTime :: NominalDiffTime -> Time -> Time
addTime diff (Exact v) = Exact $ addUTCTime diff <$> v

-- | If the time has occurred yet, returns the time, otherwise returns Nothing.
-- In the case that the time is negative infinity, 
occurrence :: Time -> IO (Maybe KnownTime)
occurrence Always = return (Just NegativeInfinity)
occurrence Never  = return Nothing
occurrence (Exact r) = do
    now <- getCurrentTime
    t <- IVar.nonblocking r
    return $ case t of
        Just x | now >= x -> Just (Known x)
        _                 -> Nothing

-- | Do not return before the given time has occurred.  Will happily block forever
-- when the time is positive infinity.
waitFor :: Time -> IO ()
waitFor Always = return ()
waitFor Never  = forever $ threadDelay maxBound
waitFor (Exact r) = do
    t <- IVar.blocking r
    now <- getCurrentTime
    let seconds = toRational $ t `diffUTCTime` now
    safeDelay seconds

-- Delay a given number of seconds, even if the microseconds would overflow
-- a machine integer.
safeDelay :: Rational -> IO ()
safeDelay secs = do
    let micro = 1000000 * secs
    if micro > tooBig
        then threadDelay maxBound >> safeDelay (secs - tooBig)
        else threadDelay (ceiling micro)
    where
    tooBig = fromIntegral (maxBound :: Int)

instance Eq Time where
    a == b = compare a b == EQ

instance Ord Time where
    compare Always Always = EQ
    compare Always _      = LT
    compare _      Always = GT

    compare Never Never   = EQ
    compare Never _       = GT
    compare _     Never   = LT

    compare (Exact r) (Exact r') = unsafePerformIO . IVar.blocking $ do
        a <- (Left <$> r) `mappend` (Right <$> r')
        b <- (Right <$> r') `mappend` (Left <$> r)
        return $ case (a,b) of
            (Left x, Left _)   -> LT
            (Right y, Right _) -> GT
            (Left x, Right y)  -> compare x y
            (Right y, Left x)  -> compare x y

    
    min Always _ = Always
    min _ Always = Always
    min Never y  = y
    min x Never  = x
    min (Exact r) (Exact r') = Exact $ do
        a <- (Left <$> r) `mappend` (Right <$> r')
        b <- (Right <$> r') `mappend` (Left <$> r)
        return $ case (a,b) of
            (Left x, Left _)   -> x
            (Right y, Right _) -> y
            (Left x, Right y)  -> min x y
            (Right y, Left x)  -> min x y
    
    max Always y = y
    max x Always = x
    max Never y  = Never
    max x Never  = Never
    max (Exact r) (Exact r') = Exact . join $ do
        a <- (Left <$> r) `mappend` (Right <$> r')
        b <- (Right <$> r') `mappend` (Left <$> r)
        return $ case (a,b) of
            (Left x, Left _)   -> r'
            (Right y, Right _) -> r
            (Left x, Right y)  -> return $ max x y
            (Right y, Left x)  -> return $ max x y

data Future a = Future {
        time :: Time, 
        -- ^ The time a future occurred.
        value :: a 
        -- ^ The value of the occurrence.  If the future is in the future,
        -- this might block until the value is known (but it is not guaranteed
        -- to block until the time, use waitFor if you want to be sure).
    }

makeFuture :: Time -> a -> Future a
makeFuture = Future

instance Functor Future where
    fmap f (Future t x) = Future t (f x)

instance Monad Future where
    return = Future Always
    Future t a >>= f =
        let Future t' b = f a
        in Future (max t t') b

instance Applicative Future where
    pure = return
    (<*>) = ap

instance Monoid (Future a) where
    mempty = Future Never (error "Check the clock, I don't think it will say Infinity yet")
    mappend f f' = fst (order f f') 

instance MonadPlus Future where
    mzero = mempty
    mplus = mappend

-- | @order a b@ returns the a tuple whose first element is whichever of @a@,
-- @b@ that occurs first, and its second element is the other one.
order :: Future a -> Future a -> (Future a, Future a)
order f@(Future t x) f'@(Future t' x') = (Future (min t t') lesser, Future (max t t') greater)
    where
    (lesser,greater) | t <= t'   = (x,x')
                     | otherwise = (x',x)

-- | Another form of order.  This one probably admits a more efficient
-- implementation than the others (but isn't currently implemented
-- that way).
order' :: Future a -> Future a -> Future (a, Future a)
order' f f' = fmap (\x -> (x,g)) l
    where
    (l,g) = order f f'

-- | Create a future and a sink.  The future becomes defined at the first time
-- the sink is called.  If the sink is called more than once, it throws an
-- exception.
sinkFuture :: IO (Future a, a -> IO ())
sinkFuture = do
    var <- IVar.new
    let sink x = do
            now <- getCurrentTime
            IVar.write var (now,x)
    let rd = IVar.read var
    x <- unsafeInterleaveIO . IVar.blocking $ snd <$> rd
    return $ (Future (Exact $ fst <$> rd) x, sink)

waitFuture :: Future a -> IO a
waitFuture (Future time value) = do
    waitFor time
    return value
