-- maybe.hs

module Maybe where

f :: Bool -> Maybe Int
f False = Just 0
f _ = Nothing