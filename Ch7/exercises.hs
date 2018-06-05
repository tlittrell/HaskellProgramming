-- exercises.hs

module Exercises where

digit :: Integral a => a -> a -> a
digit x d = snd . flip divMod d . fst $ divMod x d

g :: (a -> b) -> (a,c) -> (b,c)
g f (a,c) = (f a, c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

main = do
    print (roundTrip 4)
    print (id 4)