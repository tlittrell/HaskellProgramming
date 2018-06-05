-- exercises.hs

module Exercises where

import Data.List (intersperse)

mc91 :: Integer -> Integer
mc91 n
    | n > 100  = n - 10
    | n <= 100 = mc91 $ mc91 (n+11)

wordNumber :: Int -> String
wordNumber 0 = "zero"
wordNumber 1 = "one"
wordNumber 2 = "two"
wordNumber 3 = "three"
wordNumber 4 = "four"
wordNumber 5 = "five"
wordNumber 6 = "six"
wordNumber 7 = "seven"
wordNumber 8 = "eight"
wordNumber 9 = "nine"
wordNumber _ = error "not a digit"

digits :: Int -> [Int]
digits 0 = []
digits n = digits (div n 10) ++ [mod n 10]

digitToWord :: Int -> String
digitToWord n = concat $ intersperse "-" $ map wordNumber $ digits n
