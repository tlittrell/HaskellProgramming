import Data.Char
import Data.List

type Message = String
type CodeWord = String

testString = "MEET AT DAWN"
expectedString = "MPPR AE OYWY"
code = "ALLY"

takeCycle :: Message -> CodeWord -> String
takeCycle m c = take (length m) (cycle c)

getCodeRepetition :: Message -> CodeWord -> Message
getCodeRepetition m c = flip takeCycle c $ concat $ words m

encode :: Message -> CodeWord -> String
encode m c = map chr $ zipWith (\x y -> (x+y) `rem` 26 + ord 'A') numMessage numCode
    where numMessage = map ord $ concat $ words m
          numCode = map ord $ getCodeRepetition m c

-- as-patterns
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf p@(x:xs) (y:ys) = if x == y then isSubseqOf xs ys else isSubseqOf p ys

isSubseqOfTests = do
    print $ isSubseqOf "blah" "blahwoot" == True
    print $ isSubseqOf "blah" "wootblah" == True
    print $ isSubseqOf "blah" "wboloath" == True
    print $ isSubseqOf "blah" "wootbla"  == False
    print $ isSubseqOf "blah" "halbwoot" == False
    print $ isSubseqOf "blah" "blawhoot" == True

-- Language exercises
capitalizeWord :: String -> String
capitalizeWord (x:xs) = [toUpper x] ++ xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = zip (words xs) (map capitalizeWord $ words xs)

capitalizeWordsTests = do
    print $ capitalizeWords "hello word"
