module Frag.Kernel 
    ( Writer, Reader, Request
    , RequestStream
    , mergeRequests, runKernel
    , monotoneRequests
    )
where

import Frag.Time
import Control.Monad (join)
import Data.Monoid

type Writer a = a -> IO ()
type Reader a = IO a
type Request = IO ()

data RequestCell = Nil | Cons Request RequestStream
newtype RequestStream = RStream (Future RequestCell)

instance Monoid RequestStream where
    mempty = RStream (return Nil)
    mappend = mergeRequests

mergeRequests :: RequestStream -> RequestStream -> RequestStream
mergeRequests (RStream as) (RStream bs) = 
    RStream . join . fmap (uncurry mergeCell) $ order' as bs
    where
    mergeCell Nil fut = fut
    mergeCell (Cons a (RStream fut)) fut' = 
        return $ Cons a (mergeRequests (RStream fut) (RStream fut'))

runKernel :: RequestStream -> IO ()
runKernel (RStream fut) = do
    cell <- waitFuture fut
    case cell of
        Nil -> return ()
        Cons a ss -> a >> runKernel ss

monotoneRequests :: Writer Time -> Reader a -> Writer a -> [Time] -> RequestStream
monotoneRequests timechan rchan wchan = go
    where
    go [] = RStream (return Nil)
    go (t:ts) = 
        RStream (makeFuture t (Cons (timechan t >> rchan >>= wchan) (go ts)))
