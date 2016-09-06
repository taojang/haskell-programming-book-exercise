{-# LANGUAGE FlexibleContexts #-}
module Chapter21 where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers


newtype Identity a = Identity a deriving (Show, Eq, Ord)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Foldable Identity where
  foldr f x (Identity y) = f y x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

checkIdentityTraversable :: IO ()
checkIdentityTraversable = do
  let trigger = undefined :: Identity (Int, Int, [Int])
  quickBatch (traversable trigger)

-------------------------------------------

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

concat' :: List a -> List a -> List a
concat' xs Nil = xs
concat' Nil ys = ys
concat' (Cons x xs) ys = Cons x (concat' xs ys)

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

instance Foldable List where
  foldr _ z Nil = z
  foldr f z (Cons x xs) = f x (foldr f z xs) -- foldr f (f x z) xs

-- instance Foldable List where
--   foldMap f Nil = mempty
--   foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x  <- arbitrary
    xs <- arbitrary
    frequency [(1, pure Nil), (1, pure (Cons x xs))]

instance Eq a => EqProp (List a) where
  (=-=) = eq

checkListTraversable :: IO ()
checkListTraversable = do
  let trigger = undefined :: List (Int, Int, [Int])
  quickBatch (traversable trigger)

-------------------------------------------

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  _ <*> (Constant x) = Constant x

instance Foldable (Constant a) where
  foldr _ x _ = x

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure $ Constant x

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq


checkConstTraversable :: IO ()
checkConstTraversable = do
  let trigger = undefined :: Constant Int (Int, Int, [Int])
  quickBatch (traversable trigger)


-------------------------------------------

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S x y) = S (fmap f x) (f y)

instance Applicative n => Applicative (S n) where
  pure x = S (pure x) x
  (S nf f) <*> (S nx x) = S (nf <*> nx) (f x)

instance Foldable n => Foldable (S n) where
  foldMap f (S nx x) = foldMap f nx <> f x

instance Traversable n => Traversable (S n) where
  traverse f (S nx x) = S <$> traverse f nx <*> f x

instance (Arbitrary b, Arbitrary (n b)) => Arbitrary (S n b) where
  arbitrary = do
    nb <- arbitrary
    b  <- arbitrary
    return $ S nb b

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

checkSTraversable :: IO ()
checkSTraversable = do
  let trigger = undefined :: S Identity (Int, Int, [Int])
  quickBatch (traversable trigger)

-------------------------------------------

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x)  = Leaf <$> f x
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    x <- arbitrary
    l <- arbitrary
    r <- arbitrary
    elements [Empty, Leaf x, Node l x r]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

checkTreeTraversable :: IO ()
checkTreeTraversable = do
  let trigger = undefined :: Tree (Int, Int, [Int])
  quickBatch (traversable trigger)
