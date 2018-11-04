import Data.Monoid
import Test.QuickCheck

-- 1

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    (<>) _ _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
    (<>) (Identity a) (Identity b) = Identity (a <> b)

identityGen :: (Arbitrary a, Semigroup a) => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return (Identity a)

instance (Arbitrary a, Semigroup a) => Arbitrary (Identity a) where
    arbitrary = identityGen

type IdentityStringAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two a b) (Two c d) = Two (a <> c) (b <> d)

twoGen :: (Arbitrary a, Semigroup a, Arbitrary b, Semigroup b) => Gen (Two a b)
twoGen = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Arbitrary a, Semigroup a, Arbitrary b, Semigroup b) => Arbitrary (Two a b) where
    arbitrary = twoGen

type TwoStringIntAssoc = (Two String (Sum Int)) -> 
                         (Two String (Sum Int)) -> 
                         (Two String (Sum Int)) -> 
                         Bool

-- 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (<>) (BoolConj True)  (BoolConj True)  = BoolConj True 
    (<>) _                (BoolConj False) = BoolConj False
    (<>) (BoolConj False) _                = BoolConj False 

boolConjGen :: Gen BoolConj
boolConjGen = do
    a <- (arbitrary :: Gen Bool)
    return (BoolConj a)

instance Arbitrary BoolConj where
    arbitrary = boolConjGen

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (<>) (BoolDisj True)   _                = BoolDisj True
    (<>) _                 (BoolDisj True)  = BoolDisj True
    (<>) (BoolDisj False)  (BoolDisj False) = BoolDisj False

instance Arbitrary BoolDisj where
    arbitrary = do
        a <- (arbitrary :: Gen Bool)
        return $ BoolDisj a

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    (<>) (Fst a) (Fst b) = Fst b
    (<>) (Fst a) (Snd b) = Snd b
    (<>) (Snd a) (Fst b) = Snd a
    (<>) (Snd a) (Snd b) = Snd a

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = orGen

type OrGenStringString = (Or String String) -> (Or String String) -> (Or String String) -> Bool

-- Run tests
main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdentityStringAssoc)
    quickCheck (semigroupAssoc :: TwoStringIntAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrGenStringString)