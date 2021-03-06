module Searching where

import Data.Maybe

--Exercise 2.1-3
linearSearch :: (a -> Bool) -> [a] -> Maybe a
linearSearch _ [] = Nothing
linearSearch pred (a:as)
    | pred a    = Just a
    | otherwise = linearSearch pred as

linearSearchI :: (a -> Bool) -> [a] -> Maybe Int
linearSearchI pred as = f 0 as where
    f _ [] = Nothing
    f n (x:xs)
        | pred x    = Just n
        | otherwise = f (n + 1) xs

binarySearch :: Ord a =>  a -> [a] -> Maybe a
binarySearch seeking (a:[])
    | a == seeking  = Just a
    | otherwise     = Nothing
binarySearch seeking ls = let 
    mi = (length ls) `div` 2
    mid = ls !! mi
    in if mid == seeking
       then Just mid
       else if mid < seeking
            then binarySearch seeking (drop mi ls)
            else binarySearch seeking (take mi ls)
