-- zip.hs

module Zip where

myZip :: [a] -> [b] -> [(a,b)]
myZip [] []         = []
myZip [] (x:s)      = []
myZip (x:xs) []     = []
myZip (x:xs) (y:ys) = [(x,y)] ++ myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] [] = []
myZipWith _ [] (x:xs) = []
myZipWith _ (x:xs) [] = []
myZipWith f (x:xs) (y:ys) = [f x y] ++ myZipWith f xs ys

myZip2 :: [a] -> [b] -> [(a,b)]
myZip2 = zipWith (,)

main :: IO ()
main = do
    print $ myZip [1..3] [4..6] == zip [1..3] [4..6]
    print $ myZip [1,2] [4..6] == zip [1,2] [4..6]
    print $ myZipWith (+) [1,2,3] [10,11,12] == zipWith (+) [1,2,3] [10,11,12]