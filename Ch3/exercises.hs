-- exercises.hs

module Exercises where

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" !! x

rvrs :: String -> String
rvrs s = x ++ " " ++ y ++ z
  where x = drop 9 s
        y = take 3 $ drop 6 s
        z = take 5 s

main :: IO()
main = print $ rvrs "curry is awesome"
