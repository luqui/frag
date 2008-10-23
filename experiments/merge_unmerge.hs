
type Unmerger = forall b. [b] -> ([b],[b])

mergeUnmerge :: (Ord a) => [a] -> [a] -> ([a], Unmerger)
mergeUnmerge [] bs = (bs, \r -> ([],r))
mergeUnmerge as [] = (as, \r -> (r,[]))
mergeUnmerge (a:as) (b:bs)
    | a <= b    = let (rest, unm :: Unmerger) = mergeUnmerge as (b:bs) 
                  in (a:rest, \ (a':rest') -> let (as',bs') = unm rest' in (a':as',bs'))
    | otherwise = let (rest, unm :: Unmerger) = mergeUnmerge (a:as) bs
                  in (b:rest, \ (b':rest') -> let (as',bs') = unm rest' in (as',b':bs'))

prop_inverses xs ys = 
    let (combined, unmerge :: Unmerger) = mergeUnmerge xs ys
        (xs',ys')                       = unmerge combined
    in xs == xs' && ys == ys'
    where
    types = (xs :: [Integer])
