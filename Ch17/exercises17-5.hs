import Test.QuickCheck
import Test.QuickCheck.Property.Functor
import Data.Monoid

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)

instance Applicative Identity where
    pure a = Identity a
    (<*>) (Identity f) (Identity a) = Identity (f a)

-- Constant

newtype Constant a b = Constant { getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = (Constant a)

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>) (Constant a) (Constant b) = Constant (a <> b)


-- Fixer Upper
fixerUpper :: IO ()
fixerUpper = do
    putStrLn $ show $ const <$> Just "Hello" <*> Just"world"
    putStrLn $ show $ (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]