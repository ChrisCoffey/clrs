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
