import Control.Applicative
import Data.List

-- 1
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1..3] [4..6])

-- 2
y :: Maybe Integer
y = lookup 3 $ zip [1..3] [4..6]

z :: Maybe Integer
z = lookup 2 $ zip [1..3] [4..6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
