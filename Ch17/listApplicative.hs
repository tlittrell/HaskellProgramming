import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- List

data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance (Eq a) => EqProp (List a) where (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Cons a (Cons b Nil), Nil]

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid (List a) where
    mempty = Nil
    mappend xs Nil = xs
    mappend Nil xs = xs
    mappend (Cons x xs) ys = Cons x (xs <> ys) 

instance Applicative List where
    pure a = Cons a Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) xs = (f <$> xs) <> (fs <*> xs)

listTestType = undefined :: List (Sum Int,String,[Int])

main :: IO ()
main = do
    quickBatch (functor listTestType)
    quickBatch (monoid listTestType)
    quickBatch (applicative listTestType)