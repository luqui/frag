module Frag.Knot 
    ( Knot, makeKnot, runKnot )
where

import Control.Arrow hiding (pure)

type Unmerger = forall a. [a] -> ([a],[a])

mergeUnmerge :: (Ord a) => [a] -> [a] -> ([a], Unmerger)
mergeUnmerge [] bs = (bs, \bs' -> ([], bs'))
mergeUnmerge as [] = (as, \as' -> (as', []))
mergeUnmerge (a:as) (b:bs)
    | a <= b    = let (rest, unmerger :: Unmerger) = mergeUnmerge as (b:bs)
                  in (a:rest, \ (a':rest') -> 
                    let (as',bs') = unmerger rest' in (a':as',bs'))
    | otherwise = let (rest, unmerger :: Unmerger) = mergeUnmerge (a:as) bs
                  in (b:rest, \ (b':rest') ->
                    let (as',bs') = unmerger rest' in (as',b':bs'))

prop_mergeUnmerge_inverses xs ys =
    let (merged, unmerger :: Unmerger) = mergeUnmerge xs ys
        (xs',ys') = unmerger merged
    in xs == xs' && ys == ys'
    where
    types = xs :: [Integer]


newtype Knot req resp a = Knot { unKnot :: ([req], [resp]) -> (([req],[resp]), a) }

instance Functor (Knot req resp) where
    fmap f (Knot c) = Knot ((fmap . second) f c)

instance (Ord req, Ord resp) => Monad (Knot req resp) where
    return x = Knot (\r -> (r,x))
    m >>= f = Knot $ \(req,resp) -> 
        let ((reqm,respm),a) = unKnot m     (inmreq,inmresp)
            ((reqf,respf),b) = unKnot (f a) (infreq,infresp)
            (reqMerged,  reqUnmerge  :: Unmerger) = mergeUnmerge reqm  reqf
            (respMerged, respUnmerge :: Unmerger) = mergeUnmerge respm respf
            (inmreq,  infreq)  = respUnmerge req
            (inmresp, infresp) = respUnmerge resp
        in ((reqMerged, respMerged), b)

runKnot :: Knot req resp a -> a
runKnot k = let ((req,resp),a) = unKnot k (req,resp) in a

-- There should be exactly as many returned responses as there are
-- incoming requests and vice versa.
makeKnot :: (([req],[resp]) -> (([req],[resp]), a)) -> Knot req resp a
makeKnot = Knot
