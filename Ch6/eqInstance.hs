-- eqInstance.hs

module EqInstance where

data Trivial = Trivial

instance Eq Trivial where
  Trivial == Trivial = True

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show, Enum)

data Date = Date DayOfWeek Int deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _     = False

-- can overwrite ordering from data declaration
instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _ Fri   = LT
  compare _ _     = EQ

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
       weekday == weekday' && dayOfMonth == dayOfMonth'

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn int1) (TisAn int2) =
          int1 == int2

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two int1 int2) (Two int3 int4) =
          int1 == int3 && int2 == int4

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt int1) (TisAnInt int2) = int1 == int2
  (==) (TisAString str1) (TisAString str2) = str1 == str2
  (==) _ _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair v1 v2) (Pair v3 v4) = v1 == v3 && v2 == v4

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple m n) (Tuple c d) = m == c && n == d
