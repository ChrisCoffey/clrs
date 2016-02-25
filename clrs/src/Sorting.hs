module Sorting where

--basic impl
insertionSort :: Ord a => [a] -> [a]
insertionSort [a] = [a]
insertionSort (a:as) = insert a $ insertionSort as where
    insert x []         = [x]
    insert x all@(y:ys)
        | x < y         = x:all
        | otherwise     = y:(insert x ys)

-- Exercise 2.1-1
-- Sort steps via insertion sort
-- 0: [31,41,59,26,41,58]
-- 1: [31,41,59,26,41,58]
-- 2: [31,41,59,26,41,58]
-- 3: [31,41,26,59,41,58] -> [31,26,41,59,41,58] -> [26,31,41,59,41,58] 
-- 4: [26,31,41,41,59,58] 
-- 5: [26,31,41,41,58,59] 

--Exercise 2.1-2
-- insertion sort descending order
insertionSort' :: Ord a => [a] ->[a]
insertionSort' [a] = [a]
insertionSort' (a:as) = insert a $ insertionSort' as where
    insert x []         = [x]
    insert x all@(y:ys)
        | x > y         = x:all -- Only change is flipping the relationlal operator
        | otherwise     = y:(insert x ys)

selectionSort :: Ord a => [a] -> [a]
selectionSort ls = go ls [] where
    removeN n (a:as) 
        | n == a = as
        | otherwise = a: (removeN n as)
    go [] acc = reverse acc
    go ls acc = go (removeN (minimum ls) ls) ((minimum ls):acc) 

mergeSort :: Ord a => [a] -> [a]
mergeSort ls = mSort ls where
    merge [] bs = bs
    merge as [] = as
    merge at@(a:as) bt@(b:bs)
        | a < b     = a:(merge as bt)
        | b < a     = b:(merge at bs)
        | otherwise = a:(merge as bt)
    mSort (a:[]) = [a]
    mSort xs = let
        n = (\x -> x `div` 2) . length $ xs 
        p = mSort . take n $ xs
        q = mSort . drop n $ xs
        in merge p q
        
 
