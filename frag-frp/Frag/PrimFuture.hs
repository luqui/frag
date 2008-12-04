module Frag.PrimFuture 
    ( PrimFuture
    , Listener, newListener, newSink, wait
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
    listenerChan :: Chan (Unique, GHC.Prim.Any)
}

newListener :: IO Listener
newListener = do
    chan <- newChan
    return $ Listener chan

newSink :: Listener -> IO (PrimFuture a, a -> IO ())
newSink (Listener chan) = do
    ident <- newUnique
    return (PrimFuture ident unsafeCoerce, 
            \x -> writeChan chan (ident, unsafeCoerce x))

wait :: Listener -> [PrimFuture a] -> IO [a]
wait listener futures = do
    (ident,fdat) <- readChan (listenerChan listener)
    let ans = [ pfCast fut fdat | fut <- futures, pfID fut == ident ]
    case ans of
        [] -> wait listener futures
        _  -> return ans
