module Frag.PrimFuture where

import Data.Unique
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import GHC.Conc (unsafeIOToSTM)
import Data.Time.Clock
import Unsafe.Coerce
import qualified GHC.Prim
import Data.Maybe
import Control.Monad (unless)
import Control.Arrow ((>>>), second)
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import Control.Monad.Fix

newtype Time = Time UTCTime
    deriving (Eq, Ord)

data PrimFuture a where
    Ident   :: FutureID -> PrimFuture a
    Exact   :: Time -> a -> PrimFuture a
    FMap    :: (a -> b) -> PrimFuture a -> PrimFuture b
    MConcat :: [PrimFuture a] -> PrimFuture a

instance Functor PrimFuture where
    fmap = FMap

instance Monoid (PrimFuture a) where
    mempty = MConcat []
    mappend a b = MConcat [a,b]
    mconcat = MConcat

type FutureID = Unique   -- ID for a sink-generated future
type CallbackID = Unique -- ID for a callback which generates a single result

data FHeap a = FHeap {
    -- map futures to callbacks which depend on them
    fhToCallback :: Map.Map FutureID [CallbackID],
    -- map callbacks to generators of results, together with 
    -- related callbacks (which should be removed)
    fhCallbacks  :: Map.Map CallbackID (GHC.Prim.Any -> a, [CallbackID])
}

insertCallback :: CallbackID -> (GHC.Prim.Any -> a) -> [CallbackID] -> FHeap a -> FHeap a
insertCallback cbid cb related fheap = 
    fheap { fhCallbacks = Map.insert cbid (cb, related) (fhCallbacks fheap) }

insertAssoc :: FutureID -> CallbackID -> FHeap a -> FHeap a
insertAssoc fid cbid fheap = 
    fheap { fhToCallback = Map.insertWith (++) fid [cbid] (fhToCallback fheap) }

callbackFHeap :: GHC.Prim.Any -> [CallbackID] -> FHeap a -> (FHeap a, [a])
callbackFHeap futdata callbacks fheap = 
    (fheap { fhCallbacks = callbacks' }, results)
    where
    (generators, related) = unzip [ (gen, rs) 
                                  | cbid <- callbacks
                                  , Just (gen,rs) <- return $ Map.lookup cbid (fhCallbacks fheap) ]
    callbacks' = Map.difference (fhCallbacks fheap) 
                                (Map.fromList [ (k,()) | k <- concat related ])
    results = map ($ futdata) generators
    

activateFHeap :: FutureID -> GHC.Prim.Any -> FHeap a -> (FHeap a, [a])
activateFHeap futid futdata fheap = (fheap' { fhToCallback = toCallback' }, results)
    where
    callbacks = fromMaybe [] $ Map.lookup futid (fhToCallback fheap)
    toCallback' = Map.delete futid (fhToCallback fheap)
    (fheap', results) = callbackFHeap futdata callbacks fheap

waitFor :: Time -> IO ()
waitFor (Time time) = do
    now <- getCurrentTime
    if time < now
        then return ()
        else do
            let diff = realToFrac $ diffUTCTime time now
            threadDelay . ceiling $ 1000000 * diff       

makeTimer :: Time -> IO (STM ())
makeTimer (Time time) = do
    now <- getCurrentTime
    if time < now
        then return (return ())
        else do
            var <- atomically $ newTVar False
            forkIO $ do
                waitFor (Time time)
                atomically $ writeTVar var True
            return $ do
                v <- readTVar var
                unless v retry


data PrimFutureHeap a = PrimFutureHeap {
    pfhChan   :: TChan (FutureID, GHC.Prim.Any),
    pfhFHeap  :: TVar (FHeap a),
    pfhExacts :: TVar (Map.Map Time [(FutureID, GHC.Prim.Any)])
}

newUniqueSTM :: STM Unique
newUniqueSTM = unsafeIOToSTM newUnique

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar tv f = writeTVar tv . f =<< readTVar tv

insertFuture' :: forall a b. PrimFutureHeap a -> [CallbackID] -> (b -> a) -> PrimFuture b -> STM [CallbackID]
insertFuture' pfheap related = go
    where
    go :: forall b. (b -> a) -> PrimFuture b -> STM [CallbackID]
    go trans (Ident fid) = do
        cbid <- newUniqueSTM
        modifyTVar (pfhFHeap pfheap) $
            insertCallback cbid (trans . unsafeCoerce) related >>>
            insertAssoc fid cbid
        return [cbid]
    go trans (Exact time x) = do
        fid <- newUniqueSTM
        modifyTVar (pfhExacts pfheap) $ Map.insertWith (++) time [(fid, unsafeCoerce x)]
        go trans (Ident fid)
    go trans (FMap f pf) = go (trans . f) pf
    go trans (MConcat pfs) = fmap concat $ mapM (go trans) pfs
    
insertPrimFuture :: PrimFutureHeap a -> PrimFuture a -> IO ()
insertPrimFuture pfheap pf = mdo 
    related <- atomically $ insertFuture' pfheap related id pf
    return ()

newPrimFuture :: PrimFutureHeap a -> IO (PrimFuture a, a -> IO ())
newPrimFuture pfheap = do
    fid <- newUnique
    return (Ident fid, \x -> atomically $ writeTChan (pfhChan pfheap) (fid, unsafeCoerce x))

withTimeSTM :: STM a -> STM (Time,a)
withTimeSTM stm = do
    r <- stm
    t <- unsafeIOToSTM getCurrentTime
    return (Time t,r)

waitPrimFuture :: PrimFutureHeap a -> IO (Time, [a])
waitPrimFuture pfheap = do
    exacts <- atomically $ readTVar (pfhExacts pfheap)
    timer <- if Map.null exacts
                then return retry
                else do
                    let (time,vs) = Map.findMin exacts
                    t <- makeTimer time
                    return $ t >> return (time,vs)
    atomically $ do
        (time,assocs) <- timer `orElse` withTimeSTM (fmap (:[]) (readTChan (pfhChan pfheap)))
        heap0 <- readTVar (pfhFHeap pfheap)
        let (heap',results) = 
                foldr (\(futid,fdata) (heap,results) -> 
                            second (++results) $ activateFHeap futid fdata heap)
                    (heap0,[]) assocs
        writeTVar (pfhFHeap pfheap) heap'
        return (time,results)
