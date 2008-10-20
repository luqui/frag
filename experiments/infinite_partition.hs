import Control.Arrow
import Debug.Trace

trace' :: (Show a) => a -> a
trace' x = trace (show x) x

traceF :: (Show a) => a -> Bool
traceF x = trace ("*" ++ show x ++ "*") False

partitions :: (Show a) => [(Integer, a)] -> [[a]]
partitions = cl0 . map (first integralBits)
    where
    cl0 xs = let (nulls,nonnulls) = partitionEithers (map nullP xs)
             in nulls : cl1 nonnulls
    cl1 xs = let (trues,falses) = partitionEithers (map tfP xs)
             in cl0 trues `interleave` cl1 falses

    nullP ([],x) = Left x
    nullP (b:bs,x) = Right (b,bs,x)
    tfP (True,bs,x) = Left (bs,x)
    tfP (False,b:bs,x) = Right (b,bs,x)

interleave [] ys = ys
interleave (x:xs) ys = x:interleave ys xs

partitionEithers :: [Either b c] -> ([b],[c])
partitionEithers = foldr (\x t -> 
    either (\z -> first (z:) t) (\z -> second (z:) t) x) 
    ([],[])

integralBits :: (Integral a) => a -> [Bool]
integralBits n = go n []
    where 
    go 0 = id
    go x = let (q,r) = quotRem x 2 in (toBool r :) . go q
    toBool = (== 1)
