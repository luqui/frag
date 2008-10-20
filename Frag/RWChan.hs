module Frag.RWChan 
    ( RChan, newRChan, readRChan
    , WChan, newWChan, writeWChan
    )
where

import Data.IORef
import Control.Concurrent.MVar
import Control.Applicative
import System.IO.Unsafe (unsafeInterleaveIO)

-- Write continuously, read discretely
newtype RChan a = RChan (IORef [a])

-- Read continuously, write discretely
newtype WChan a = WChan (MVar (Stream a))

newtype Stream a = Stream (MVar (a, Stream a))



newRChan :: [a] -> IO (RChan a)
newRChan = fmap RChan . newIORef

readRChan :: RChan a -> IO (Maybe a)
readRChan (RChan ref) = do
    atomicModifyIORef ref $ \xs ->
        case xs of
            []     -> ([], Nothing)
            (a:as) -> (as, Just a)


newWChan :: IO ([a], WChan a)
newWChan = do
    stream <- Stream <$> newEmptyMVar
    wchan <- WChan <$> newMVar stream
    xs <- readStream stream
    return (xs, wchan)

readStream :: Stream a -> IO [a]
readStream (Stream var) = unsafeInterleaveIO $ do
    (x, chan') <- readMVar var
    xs <- readStream chan'
    return (x:xs)

writeWChan :: WChan a -> a -> IO ()
writeWChan (WChan ref) x = do
    nextCell <- Stream <$> newEmptyMVar
    Stream stream <- takeMVar ref
    putMVar stream (x,nextCell)
    putMVar ref nextCell
