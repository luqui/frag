module Frag.PreBehavior where

import Control.Arrow hiding (pure)
import Frag.Time

newtype PreBehavior b c 
    = PreBehavior { runPreBehavior :: ([b],[Time]) -> ([Time],[c]) }

instance Functor (PreBehavior b) where
    fmap f (PreBehavior a) = PreBehavior (fmap (second (fmap f)) a)

instance Arrow PreBehavior where
    arr f = PreBehavior (\(bs,ts) -> (ts, map f bs))
    f >>> g = PreBehavior $ \(~(bs,dreq)) ->   -- << watch out for lazy biteyness.  This might be one of those cases...
        let (creq, ds) = runPreBehavior g (cs,dreq)
            (breq, cs) = runPreBehavior f (bs,creq)
        in (breq,ds)
    first f = PreBehavior $ \(~(bds,creqs)) ->
        let (bs,ds) = unzip bds
            (breqs,cs) = runPreBehavior f (bs, creqs)
        in (breqs, zip cs ds)

instance ArrowLoop PreBehavior where
    loop f = PreBehavior $ \(~(bs,creqs)) ->
        let bds = zip bs ds
            (breqs, cds) = runPreBehavior f (bds, creqs)
            (cs,ds) = unzip cds
        in (breqs, cs)

-- There's an ArrowChoice instance, but it's really hard.  So... fuck it.
-- These Arrow instances are just for convenience anyway.

