-- greetIfCool.hs

module GreetIfCool1 where

greetIfCool :: String -> IO()
greetIfCool coolness =
  if cool
    then putStrLn "eyyyy. What's shakin'"
  else
    putStrLn "pshhhh"
  where cool =
          coolness == "downright frosty yo"

greetIfCool2 :: String -> IO()
greetIfCool2 coolness =
  if v
    then putStrLn "eyyyy. What's shakin'"
  else
    putStrLn "pshhhh"
  where v =
          coolness == "downright frosty yo"
