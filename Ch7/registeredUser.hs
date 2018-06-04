-- registeredUser.hs

module RegisteredUser where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber acctNum)) =
            putStrLn $ name ++ " " ++ show acctNum

myUser = Username "callen"
myAcct = AccountNumber 10456
rUser = RegisteredUser myUser myAcct