-- hof.hs

module Hof where

data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering) 
                -> Employee 
                -> Employee
                -> IO ()
employeeRank f e e' =
    case f e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither employee is the boss"
        LT -> (flip reportBoss) e e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1

oneIsTwo :: Integer -> Integer
oneIsTwo = (flip dodgy) 2