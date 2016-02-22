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

