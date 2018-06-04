-- partialApplication.hs

module PartialApplication where

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

substractStuff :: Integer -> Integer -> Integer
substractStuff x y = x - y - 10

subtractOne = substractStuff 1
