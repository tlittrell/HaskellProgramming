{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x) 

type IntToInt = Fun Int Int
type StringToString = Fun String String


-- 1

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)

type IdentityString = Identity String
type IdentityStringFC = IdentityString -> StringToString -> StringToString -> Bool

-- 2

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Pair a b)

type PairString = Pair String
type PairStringFC = PairString -> StringToString -> StringToString -> Bool

-- 3

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

type TwoStringInt = Two String Int
type TwoStringIntFC = TwoStringInt -> IntToInt -> IntToInt -> Bool

-- 4

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)

type ThreeStringIntString = Three String Int String
type ThreeStringIntStringFC = ThreeStringIntString -> StringToString -> StringToString -> Bool

-- 5

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three' a b c)

type Three'StringInt = Three' String Int
type Three'StringIntFC = Three'StringInt -> IntToInt -> IntToInt -> Bool

-- 6

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return (Four a b c d)

type FourIntStringIntString = Four Int String Int String
type FourIntStringIntStringFC = FourIntStringIntString -> StringToString -> StringToString -> Bool

-- 7

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return (Four' a b c d)

type Four'StringInt = Four' String Int
type Four'StringIntFC = Four'StringInt -> IntToInt -> IntToInt -> Bool

-- Tests
main :: IO ()
main = do
    putStrLn ""
    putStrLn "Problem 1 -- Identity"
    quickCheck (functorIdentity :: IdentityString -> Bool)
    quickCheck (functorCompose' :: IdentityStringFC)
    putStrLn ""

    putStrLn ""
    putStrLn "Problem 2 -- Pair"
    quickCheck (functorIdentity :: PairString -> Bool)
    quickCheck (functorCompose' :: PairStringFC)
    putStrLn ""

    putStrLn ""
    putStrLn "Problem 3 -- Two"
    quickCheck (functorIdentity :: TwoStringInt -> Bool)
    quickCheck (functorCompose' :: TwoStringIntFC)
    putStrLn ""

    putStrLn ""
    putStrLn "Problem 4 -- Three"
    quickCheck (functorIdentity :: ThreeStringIntString -> Bool)
    quickCheck (functorCompose' :: ThreeStringIntStringFC)
    putStrLn ""

    putStrLn ""
    putStrLn "Problem 5 -- Three'"
    quickCheck (functorIdentity :: Three'StringInt -> Bool)
    quickCheck (functorCompose' :: Three'StringIntFC)
    putStrLn ""

    putStrLn ""
    putStrLn "Problem 6 -- Four"
    quickCheck (functorIdentity :: FourIntStringIntString -> Bool)
    quickCheck (functorCompose' :: FourIntStringIntStringFC)
    putStrLn ""

    putStrLn ""
    putStrLn "Problem 7 -- Four"
    quickCheck (functorIdentity :: Four'StringInt -> Bool)
    quickCheck (functorCompose' :: Four'StringIntFC)
    putStrLn ""