module DivConq where

maxSubArrSum :: (Num a, Ord a) => [a] -> a
maxSubArrSum as = maxSum as where
    maxCrossingSum ls rs = (lSum 0 0 . reverse $ ls) + (lSum 0 0 rs) where
        lSum acc m (a:[]) = max (acc + a) m
        lSum acc m (a:as)
            | a + acc > m   = lSum (a + acc) (a + acc) as 
            | otherwise     = lSum (a + acc) m as
    maxSum (a:[]) = a
    maxSum as = let
        ln =  (length as) `div` 2
        ls = take ln as
        rs = drop ln as
        leftSum = maxSum ls
        rightSum = maxSum rs
        midSum = maxCrossingSum ls rs
        in max midSum . max leftSum $ rightSum
