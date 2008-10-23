

mergeUnmerge :: (Ord a) => [a] -> [a] -> ([a], forall b. [b] -> ([b],[b]))
mergeUnmerge [] bs = (bs, \r -> ([],r))
mergeUnmerge as [] = (as, \r -> (r,[]))
mergeUnmerge (a:as) (b:bs)
    | a <= b    = let (rest, unm :: forall b. [b] -> ([b],[b])) = mergeUnmerge as (b:bs) 
                  in (a:rest, \ (a':rest') -> let (as',bs') = unm rest' in (a':as',bs'))
    | otherwise = let (rest, unm :: forall b. [b] -> ([b],[b])) = mergeUnmerge (a:as) bs
                  in (b:rest, \ (b':rest') -> let (as',bs') = unm rest' in (as',b':bs'))
