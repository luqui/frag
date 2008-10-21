module Frag.Util where

import Prelude hiding (until)
import Frag.Time
import Frag.Behavior
import Data.Time.Clock
import Frag.RWChan
import Data.Maybe
import Control.Applicative
import Control.Concurrent
import System.IO
import Control.Monad
import System.IO.Unsafe (unsafeInterleaveIO)

makeBehaviorMonitor :: Behavior a -> IO (IO (), IO a)
makeBehaviorMonitor beh = do
    timestream <- makeTimeStream
    let times = map fst timestream
    timechan <- newRChan (map snd timestream)

    (thread, vals) <- evalBehavior beh times
    vchan <- newRChan vals
    
    let monitor = do
            now <- getCurrentTime
            join (fromJust <$> readRChan timechan)
            fromJust <$> readRChan vchan
    return (thread, monitor)

makeTimeStream :: IO ([(Time, IO ())])
makeTimeStream = unsafeInterleaveIO $ do
    rest <- makeTimeStream
    (fut, sink) <- sinkFuture
    return $ (time fut, sink ()) : rest

testBehavior :: (Show a) => String -> Behavior a -> IO ()
testBehavior filename beh = do
    (thread, mon) <- makeBehaviorMonitor beh
    forkIO thread
    fh <- openFile filename WriteMode
    forkIO $ forever $ do
        hPutStrLn fh =<< (show <$> mon)
        hFlush fh
        threadDelay 100000
    return ()
        

identity :: Behavior Double -> Behavior Double
identity = monotoneTrans1 $ \bs times -> (times, bs)

integral :: Behavior Double -> Behavior (Behavior Double)
integral = monotoneTrans1 $ \bs times ->
        case times of
            []     -> ([],[])
            (t0:_) -> 
                let requests = enumFromFollow t0 timestep times
                    integrals = scanl (+) 0 ((stepsize *) <$> bs)
                    vals = zip requests integrals
                    behs = [ monotoneTrans0 (innerB vals') | vals' <- locates times vals ]
                in (requests,behs)
    where
    innerB vals ts = snd . head <$> locates ts vals

    stepsize = 0.01
    timestep = realToFrac stepsize

    -- like enumFrom, but only outputs values which are less than some
    -- value in the list.  This is so we make finitely many requests
    -- when finitely many requests are made of us.
    enumFromFollow x step [] = []
    enumFromFollow x step (t:ts)
        | x < t = x : enumFromFollow (addTime step x) step ts
        | otherwise = enumFromFollow x step ts

    locates [] _  = []
    locates ts [] = []
    locates (t:ts) ((t',v):vs)
        | addTime timestep t < t' = locates (t:ts) vs
        | otherwise               = ((t',v):vs) : locates ts vs
