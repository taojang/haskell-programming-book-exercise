module VE where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a = Err e | Su a
  deriving (Show, Eq)

instance Functor (Validation e) where
  fmap _ (Err e) = Err e
  fmap f (Su x) = Su (f x)

instance Monoid e => Applicative (Validation e) where
  pure = Su
  Err e0 <*> Su _ = Err e0
  Err e0 <*> Err e1 = Err (e0 <> e1)
  Su _   <*> Err e0 = Err e0
  Su f   <*> Su  x  = Su (f x)

data Sum e a = First e | Second a
  deriving (Show, Eq)
