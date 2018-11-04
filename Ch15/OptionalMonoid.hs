import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

-- This code is correct but doesn't work with the latest version of Haskell
instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend (Only a) (Nada) = Only a
    mappend (Nada) (Only b) = Only b
    mappend (Only a) (Only b) = Only (a <> b)