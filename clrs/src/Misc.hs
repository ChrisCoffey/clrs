module Misc where

-- Exercise 2.1-4
binaryAddition :: [Bool] -> [Bool] -> [Bool]
binaryAddition a b = binAdd (reverse a) (reverse b) [] False where
    binAdd [] [] acc carry = carry:acc
    binAdd (x:xs) (y:ys) acc carry 
     | x && y = binAdd xs ys (carry:acc) True
     | (x || y) && carry = binAdd xs ys (False:acc) True
     | (x || y) && not carry = binAdd xs ys (True:acc) False
     | carry = binAdd xs ys (carry:acc) False
     | not x && not y && not carry = binAdd xs ys (False:acc) False

-- Computes polynomials
hornersRule :: Num a => [a] -> a -> a
hornersRule coefs x = calc coefs 0 where
    calc [] y = y
    calc (a:rest) y = calc rest (a + x * y)

-- Finding Inversions
numInversions :: Ord a => [a] -> Int 
numInversions ls = find ls where
    isGreater n ls = length . filter ((<) n) $ ls
    check [] _ = 0
    check _ [] = 0
    check low high = foldl (\acc h -> acc + (isGreater h low)) 0 high
    find (x:[]) = 0
    find xs = let
        n = (\x -> x `div` 2) . length $ xs
        p = take n xs
        q = drop n xs
        in (check p q) + (find p) + (find q)
