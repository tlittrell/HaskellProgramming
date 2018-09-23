-- record.hs

module Record where

data Person = Person { name :: String, age :: Int} deriving (Eq, Show)

papu = Person "Papu" 5