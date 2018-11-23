import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Pair
data Pair a = Pair a a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Pair a b)

instance (Eq a) => EqProp (Pair a) where (=-=) = eq

instance (Monoid a) => Monoid (Pair a) where
    mempty = Pair mempty mempty
    mappend (Pair a b) (Pair c d) = Pair (a <> c) (b <> d)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
    pure a = Pair a a
    (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

-- Two
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend (Two a a') (Two b b') = Two (a <> b) (a' <> b')

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
    pure a = Two mempty a
    (<*>) (Two a f) (Two b x) = Two (a <> b) (f x)

-- Three
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty
    mappend (Three a1 b1 c1) (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure a = Three mempty mempty a
    (<*>) (Three a b f) (Three a' b' x) = Three (a <> a') (b <> b') (f x)

-- Check
pairCheckTypeInst = undefined :: Pair (Sum Int, Sum Int, Sum Int)

twoCheckTypeMonoid = undefined :: Two (Sum Int) (String)
twoCheckTypeFunctor = undefined :: Two String (Sum Int, Sum Int, String)

threeCheckTypeCheck = undefined :: Three (Sum Int) (Product Int) (String, Sum Int, Product Int)

main :: IO ()
main = do
    putStrLn $ ""
    putStrLn $ "Pair a"
    quickBatch $ monoid pairCheckTypeInst
    quickBatch $ functor pairCheckTypeInst
    quickBatch $ applicative pairCheckTypeInst

    putStrLn $ ""
    putStrLn $ "Two a b"
    quickBatch $ monoid twoCheckTypeMonoid
    quickBatch $ functor twoCheckTypeFunctor
    quickBatch $ applicative twoCheckTypeFunctor

    putStrLn $ ""
    putStrLn $ "Three a b c"
    quickBatch $ monoid threeCheckTypeCheck
    quickBatch $ functor threeCheckTypeCheck
    quickBatch $ applicative threeCheckTypeCheck