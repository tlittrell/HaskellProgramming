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