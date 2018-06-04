-- typeInference.hs

module TypeInference1 where

f :: Num a => a -> a -> a
f x y = x + y + 3

f2 x y = x + y + 3
