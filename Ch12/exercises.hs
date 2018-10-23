import Data.Char

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord xs = if numCons > numVowels then Just (Word' xs) else Nothing
    where numVowels = length $ filter (\x -> x `elem` vowels) xs
          numCons = length [x | x <- xs, not $ x `elem` vowels] 
