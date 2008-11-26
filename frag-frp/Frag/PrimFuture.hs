module Frag.PrimFuture
    ( Time, PrimFuture, FutureMap
    , newFutureMap
    , warn, now
    , newPrimFuture, activatePrimFuture, newPrimFutureThread
    , addListener, listen
    )
where

import System.Mem.Weak
import Data.Time.Clock
import Data.Unique
import qualified GHC.Prim
import qualified Data.Map as Map
import GHC.Conc (unsafeIOToSTM)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.MVar
import Control.Monad (liftM2)
import Data.Monoid (Monoid, mappend)
import Unsafe.Coerce
import System.IO

newtype Time = Time UTCTime
    deriving (Eq, Ord)

data PrimFuture a = PrimFuture { pfIdent :: Unique }

data FutureMap r = FutureMap {
        fmMap  :: TVar (Map.Map Unique GHC.Prim.Any),
        fmChan :: TChan (Time, r)
    }

newFutureMap :: IO (FutureMap r)
newFutureMap = atomically $ do
    mp <- newTVar Map.empty
    chan <- newTChan
    return $ FutureMap { fmMap = mp, fmChan = chan }

warn :: String -> IO ()
warn = hPutStrLn stderr

now :: IO Time
now = fmap Time getCurrentTime

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar tv f = do
    val <- readTVar tv
    writeTVar tv (f val)

newPrimFuture :: FutureMap r -> IO (PrimFuture a)
newPrimFuture fm = do
    ident <- newUnique
    let pfut = PrimFuture ident
    addFinalizer pfut . atomically $ modifyTVar (fmMap fm) (Map.delete ident)
    return pfut

activatePrimFuture :: FutureMap r -> PrimFuture a -> a -> IO ()
activatePrimFuture fm pf x = do
    elem <- atomically $ do
        mp <- readTVar (fmMap fm)
        writeTVar (fmMap fm) (Map.delete (pfIdent pf) mp)
        return $ Map.lookup (pfIdent pf) mp
    case elem of
        Nothing -> do
            warn "Dead future activated!  This is not good."
            return ()
        Just f -> atomically $ do
            t <- unsafeIOToSTM now
            writeTChan (fmChan fm) $ (t, unsafeCoerce f x)

newPrimFutureThread :: FutureMap r -> IO a -> IO (PrimFuture a)
newPrimFutureThread fm thread = do
    pf <- newPrimFuture fm
    threadidvar <- newEmptyMVar
    weakpf <- mkWeakPtr pf (Just (killThread =<< takeMVar threadidvar))
    threadid <- forkIO $ do
        x <- thread
        mpf' <- deRefWeak weakpf
        case mpf' of
            Nothing -> return ()
            Just pf' -> activatePrimFuture fm pf' x
    return pf
        

addListener :: (Monoid r) => FutureMap r -> PrimFuture a -> (a -> r) -> IO ()
addListener fm pf transform = atomically $ do
    mp <- readTVar (fmMap fm)
    let newentry = case Map.lookup (pfIdent pf) mp of
                        Nothing -> transform
                        Just t' -> liftM2 mappend (unsafeCoerce t') transform
    writeTVar (fmMap fm) (Map.insert (pfIdent pf) (unsafeCoerce newentry) mp)

listen :: FutureMap r -> STM (Time, r)
listen fm = readTChan (fmChan fm)
