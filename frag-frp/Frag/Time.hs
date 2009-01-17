{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Frag.Time 
    ( Time, shiftTime, diffTime
    , negativeInfinity
    , TimeFun, time
    -- legacy adapters
    , runTimeFun
    , unsafeIOToTimeFun
    )
where

import Data.Time.Clock
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (ap)
import Control.Applicative

newtype Time = Time UTCTime
    deriving (Eq, Ord)

-- | > shiftTime seconds t = t + seconds
shiftTime :: Double -> Time -> Time
shiftTime amt (Time t) = Time $ addUTCTime (realToFrac amt) t

-- | > diffTime t t' = t - t'  -- measured in seconds
diffTime :: Time -> Time -> Double
diffTime (Time t) (Time t') = realToFrac (diffUTCTime t t')

negativeInfinity :: Time
negativeInfinity = Time $ addUTCTime (realToFrac (-1/0::Double)) (unsafePerformIO getCurrentTime)

-- | > TimeFun a = Time -> a
newtype TimeFun a = TimeFun { runTimeFun :: IO a }
    deriving (Functor, Monad)

instance Applicative TimeFun where
    pure = return
    (<*>) = ap

-- | > time = id
time :: TimeFun Time
time = TimeFun $ fmap Time getCurrentTime

unsafeIOToTimeFun :: IO a -> TimeFun a
unsafeIOToTimeFun = TimeFun
