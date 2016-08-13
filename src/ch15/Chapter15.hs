{-# LANGUAGE FlexibleInstances #-}
module Chapter15 where

import Test.QuickCheck

class Semigroup a where
  (<>) :: a -> a -> a

semigrpAssocLaw :: (Semigroup a, Eq a) => a -> a -> a -> Bool
semigrpAssocLaw x y z = (x <> y) <> z == x <> (y <> z)

data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Semigroup Trivial where
  _ <> _ = Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Semigroup [Char] where
  (<>) = (++)

---------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

type IdStrAssoc = Identity String -> Identity String -> Identity String -> Bool

---------------------------------------------------
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd x) <> _ = Snd x
  _       <> y = y

---------------------------------------------------
newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b  => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \x -> f x <> g x

---------------------------------------------------
newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ f . g

---------------------------------------------------
data Validation a b = Fa a | Su b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Fa x <> Fa y = Fa $ x <> y
  Fa x <> _    = Fa x
  Su _ <> Fa y = Fa y
  Su _ <> y    = y

---------------------------------------------------
