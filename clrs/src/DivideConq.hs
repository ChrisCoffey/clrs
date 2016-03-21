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

type Row = [a]
type Matrix = [Row]

addMatrix a b = map (\ps-> map (\(x,y)-> x + y) ps) . map (\(a, b)-> zip a b) . zip a $ b

subtractMatrix a b = map (\ps-> map (\(x,y)-> x - y) ps) . map (\(a, b)-> zip a b) . zip a $ b

-- assumes square matrices
strassmanMatrix :: (Num a, Ord a) => Matrix a -> Matrix a -> Matrix a
strassmanMatrix a b = process a b where 
    len = (`div` 2) . length $ a
    ranges = [(0, 0), (len, 0), (0, len), (len + 1, len + 1)]
    slice n matrix = let
        a = map (\r -> take len r) . take len $ matrix
        b = map (\r -> drop len r) . take len $ matrix
        c = map (\r -> take len r) . drop len $ matrix
        d = map (\r -> drop len r) . drop len $ matrix
        in (a, b, c, d)
    process mA mB
        | len
            (a11, a12, a21, a22) = slices len mA
            (b11, b12, b21, b22) = slices len mB
            s1 = subtractMatrix b12 b22
            s2 = addMatrix a11 a12
            s3 = addMatrix a21 a22
            s4 = subtractMatrix b21 b11
            s5 = addMatrix a11 a22
            s6 = addMatrix b11 b22
            s7 = subtractMatrix a12 a22
            s8 = addMatrix b21 b22
            s9 = subtractMatrix a11 a21
            s10 = addMatrix b11 b12

    
