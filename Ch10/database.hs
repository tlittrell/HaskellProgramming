-- database.hs

module Database where

import Data.Time

data DatabaseItem = DbString String 
                  | DbNumber Integer 
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [ DbDate (UTCTime (fromGregorian 1911 5 1)
                (secondsToDiffTime 34123))
              , DbNumber 9001
              , DbString "Hello world!"
              , DbDate (UTCTime
                        (fromGregorian 1921 5 1)
                        (secondsToDiffTime 34123))
              ]   

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _            = False        
              
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map (\(DbNumber i) -> i) . filter isDbNumber

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _          = False

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map (\(DbDate i) -> i) . filter isDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (+) 0 $ filterDbNumber xs