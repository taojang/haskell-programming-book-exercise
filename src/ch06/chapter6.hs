module Chapter6 where

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (TisAn x) == (TisAn y) = x == y

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (Two x y) == (Two x' y') = x == x' && y == y'

data StringOrInt = TisAnInt Integer | TisAString String

instance Eq StringOrInt where
  (TisAnInt x)   == (TisAnInt x')   = x == x'
  (TisAString x) == (TisAString x') = x == x'
  _              == _               = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (Pair x y) == (Pair x' y') = x == x' && y == y'

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (ThisOne x) == (ThisOne x') = x == x'
  (ThatOne x) == (ThatOne x') = x == x'
  _           == _            = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (Hello x)   == (Hello x')   = x == x'
  (Goodbye x) == (Goodbye x') = x == x'
  _           == _            = False

-- Chapter Ex
-- 1. c
-- 2. a b
-- 3. a
-- 4. b
-- 5. a

-- Type check
-- 1. No, no Show instance for Person
-- 2. No, no Eq instance for Mood
-- 3. a) Mood
--    b) Type mismatch
--    c) No instance Ord Mood
-- 4. both type checks, s1 :: String -> Sentence, s2 :: Sentence

-- Rocks Yeah Papu
-- 1. won't type check. "chases" not rocks True not Yeah
-- 2. type checks
-- 3. type checks
-- 4. won't. missing Ord Papu instance

-- Match the types
-- 1. no, a is more general
-- 2. no, f is Fractional, making type more general is not possible
-- 3. yes
-- 4. yes, more concrete
-- 5. yes, more concrete
-- 6. yes, more concrete
-- 7. no,  more general
-- 8. no, more general
-- 9. yes, more concrete
-- 10. yes
-- 11. no, more general

-- Type-Know-Do
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x y = fromInteger x + f y
