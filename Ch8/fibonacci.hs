-- fibonacci.hs

module Fibonacci where

fibonacci :: Integer -> Integer
fibonacci n
    | n == 0 = 1
    | n == 1 = 1
    | n > 1  = fibonacci (n-1) + fibonacci (n-2)
    | otherwise = error "n must be non-negative"