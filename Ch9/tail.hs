-- tail.hs

module Tail where

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x