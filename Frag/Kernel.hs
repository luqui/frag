module Frag.Kernel 
    ( Writer, Reader, Request
    , mergeRequests, runKernel
    , makeRequest
    )
where

import Frag.Time

type Writer a = a -> IO ()
type Reader a = IO a

type Request = Future (IO ())

mergeRequests :: [Request] -> [Request] -> [Request]
mergeRequests [] bs = bs
mergeRequests as [] = as
mergeRequests (a:as) (b:bs)
    | time a <= time b = a : mergeRequests as (b:bs)
    | otherwise        = b : mergeRequests (a:as) bs


runKernel :: [Request] -> IO ()
runKernel = mapM_ value


makeRequest :: Writer Time -> Reader a -> Writer a -> Time -> Request
makeRequest tsink r w t = makeFuture t (tsink t >> r >>= w)


