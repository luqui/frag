module Frag.PrimFuture 
    ( PrimFuture
    , Listener, newListener, destroyListener, newEvent, waitFutures
    )
where

import Data.Unique
import qualified GHC.Prim
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad (forever)
import Unsafe.Coerce

data PrimFuture a = PrimFuture {
    pfID :: Unique,
    pfCast :: GHC.Prim.Any -> a
}

instance Functor PrimFuture where
    fmap f (PrimFuture id cast) = PrimFuture id (f . cast)


data Listener = Listener {
    listenerChan :: Chan (Unique, GHC.Prim.Any),
    listenerThreads :: MVar [ThreadId]
}

newListener :: IO Listener
newListener = do
    chan <- newChan
    var <- newMVar []
    return $ Listener chan var

destroyListener :: Listener -> IO ()
destroyListener listener = do
    threads <- takeMVar (listenerThreads listener)
    mapM_ killThread threads

newEvent :: Listener -> IO a -> IO (PrimFuture a)
newEvent (Listener chan threadvar) action = do
    ident <- newUnique
    threadid <- forkIO . forever $ do
        x <- action
        writeChan chan (ident, unsafeCoerce x)
    modifyMVar_ threadvar (return . (threadid:))
    return $ PrimFuture ident unsafeCoerce

waitFutures :: Listener -> [PrimFuture a] -> IO [a]
waitFutures listener futures = do
    (ident,fdat) <- readChan (listenerChan listener)
    let ans = [ pfCast fut fdat | fut <- futures, pfID fut == ident ]
    case ans of
        [] -> waitFutures listener futures
        _  -> return ans
