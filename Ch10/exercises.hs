-- exercises.hs

module Exercises where

stops = "pbtdkg"
vowels = "aeiou"

threeTups = [('p',b,c) | b <- vowels, c <- stops]

seekritFunc :: String -> Double
seekritFunc x = (fromIntegral (sum (map length (words x))))
                / (fromIntegral (length (words x)))

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True                

myOr :: [Bool] -> Bool
myOr = foldr (||) False 

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (\y -> x == y)) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x xs-> if f x then x:xs else xs) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

main :: IO ()
main = do
    print $ myOr [True, True] == True
    print $ myOr [True, False] == True
    print $ myOr [False, True] == True
    print $ myOr [False, False] == False
    print $ not $ myAny even [1, 3, 5]
    print $ myAny odd [1, 3, 5]
    print $ myElem 1 [1..10]
    print $ not $ myElem 1 [2..10]
    print $ myReverse "blah" == "halb"
    print $ myReverse [1..5] == [5,4,3,2,1]
    print $ myMap (+1) [1..4] == [2..5]
    print $ myFilter even [1..5] == [2,4]
    print $ squish [[1,2,3],[4,5,6]] == [1..6]
    print $ squishMap (\x -> [1, x, 3]) [2] == [1,2,3]              