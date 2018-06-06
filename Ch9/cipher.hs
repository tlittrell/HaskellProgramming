-- cipher.hs

module Cipher where

import Data.Char

shiftedValue :: Int -> Int -> Int
shiftedValue x s = (x + s - 97) `mod` 26 + 97

encipher :: Char -> Int -> Char
encipher x s = chr $ shiftedValue (ord x) s

caeser :: String -> Int -> String
caeser str s = map (flip encipher s) $ map toLower str

unCaeser :: String -> Int -> String
unCaeser str s = caeser str (-s)