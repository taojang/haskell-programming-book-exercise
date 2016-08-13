module ListApp where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

concat' :: List a -> List a -> List a
concat' xs Nil = xs
concat' Nil ys = ys
concat' (Cons x xs) ys = Cons x (concat' xs ys)

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

take' :: Int -> List a ->  List a
take' _ Nil = Nil
take' x (Cons y ys)
  | x > 0  = Cons y (take' (x - 1) ys)
  | x < 1  = Nil

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

length' :: List a -> Int
length' = fold (const (+1)) 0

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)
zipWith' _ _           _           = Nil

instance Monoid (List a) where
  mempty = Nil
  mappend = concat'

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _   = Nil
  _   <*> Nil = Nil
  (Cons f Nil) <*> xs =  f <$> xs
  (Cons f fs)  <*> xs = (f <$> xs) <> (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x  <- arbitrary
    xs <- arbitrary
    frequency [(1, pure Nil), (1, pure (Cons x xs))]

instance Eq a => EqProp (List a) where
  (=-=) = eq

----------------------------------------------------

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . repeat'
  ZipList' fs <*> ZipList' ys = ZipList' $ zipWith' ($) fs ys

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

verifyZipListApp :: IO ()
verifyZipListApp = quickBatch $ applicative $ ZipList' $ Cons ("a", "b", 0 :: Int) Nil

verifyListApp :: IO ()
verifyListApp = quickBatch $ applicative $ Cons ("a", "b", 0 :: Int) Nil
