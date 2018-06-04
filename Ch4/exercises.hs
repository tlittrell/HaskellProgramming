-- exercises.hs

module Exercises where

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome str =
              str == reverse str

myAbs :: Integer -> Integer
myAbs x =
  if nonnegative
    then x
  else
    -x
  where nonnegative = x >= 0

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f (a,b) (c,d) = ((b,d),(a,c))
