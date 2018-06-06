-- exercises.hs

module Exercises where

import Data.Char

filterUpper :: String -> String
filterUpper = filter (\x -> isUpper x)

capitalizeFirst :: String -> String
capitalizeFirst []     = []
capitalizeFirst (x:xs) = [toUpper x] ++ xs

myOr :: [Bool] -> Bool
myOr []     = True
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = or $ map f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x ys = any (== x) ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ map f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

main :: IO ()
main = do
    print $ filterUpper "HbEfLrLxO" == "HELLO"
    print $ capitalizeFirst "julie" == "Julie"
    print $ myOr [True, True] == True
    print $ myOr [True, False] == True
    print $ myOr [False, True] == True
    print $ not $ myOr [False, False] == False
    print $ not $ myAny even [1, 3, 5]
    print $ myAny odd [1, 3, 5]
    print $ myElem 1 [1..10]
    print $ not $ myElem 1 [2..10]
    print $ myReverse "blah" == "halb"
    print $ myReverse [1..5] == [5,4,3,2,1]
    print $ squish [[1,2,3],[4,5,6]] == [1..6]
    print $ squishMap (\x -> [1, x, 3]) [2] == [1,2,3]
    print $ squishAgain [[1,2,3],[4,5,6]] == [1..6]