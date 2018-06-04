-- cases.hs

module Cases where

funcZ :: (Num a, Eq a) => a -> String
funcZ x = 
    case x + 1 == 1 of
        True -> "Awesome"
        False -> "wut"

pal :: (Eq a) => [a] -> String
pal xs =
    case xs == reverse xs of
        True -> "yes"
        False -> "no"

pal' :: (Eq a) => [a] -> String
pal' xs =
    case y of
        True -> "yes"
        False -> "no"
    where y = xs == reverse xs

funcC :: Ord a => a -> a -> a
funcC x y = 
    case x > y of
        True -> x
        False -> y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n =
    case even n of
        True -> n + 2
        False -> n

nums :: (Num a, Ord a) => a -> Integer
nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0