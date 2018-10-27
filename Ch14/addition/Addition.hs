module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "Hello!"

dividedBy :: Integral a => a -> a -> (a, a) 
dividedBy num denom = go num denom 0
    where go n d count = if n < d then (count, n) else go (n - d) d (count + 1)

recursiveMultiply :: (Eq a, Integral a) => a -> a -> a
recursiveMultiply n1 1 = n1
recursiveMultiply n1 n2 = n1 + (recursiveMultiply n1 (n2-1))

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            ((1::Int) + (1::Int)) > (1::Int) `shouldBe` True
        it "2 + 2 equals 4" $ do
            ((2::Int) + (2::Int)) == (4::Int) `shouldBe` True
        it "x +1 is always greater than x" $ do
            property $ \x -> x+1 > (x :: Int)
    describe "dividedBy" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5,0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
    describe "recursiveMultiply" $ do
        it "2 times 3 is 6" $ do
            recursiveMultiply 2 3 `shouldBe` (6::Int)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater