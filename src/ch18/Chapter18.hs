module Chapter18 where

import Data.Monoid ((<>))
import Control.Monad (join, liftM2)
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . (fmap f)

data Nope a = NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

---------------------------------------

data PhhhbbtttEither b a =
  Left' a
  | Right' b
  deriving (Show, Eq)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' x) = Right' x
  fmap f (Left'  x) = Left' (f x)

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (Left' f)  <*> (Left' x)  = Left' (f x)
  (Right' f) <*> _          = Right' f
  _          <*> (Right' x) = Right' x

instance Monad (PhhhbbtttEither b) where
  (Left' x)  >>= f = f x
  (Right' x) >>= _ = Right' x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, pure $ Right' x), (1, pure $ Left' y)]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

checkPhhhbbtttEither :: IO ()
checkPhhhbbtttEither =
  let x = undefined :: PhhhbbtttEither Bool (Bool, Int, Char)
  in quickBatch $ monad x


---------------------------------------

newtype Identity a = Identity a deriving (Show, Eq, Ord)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  (Identity x) >>= f = f x

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

checkIdentity :: IO ()
checkIdentity = let x = undefined :: Identity (Bool, Int, Char)
                in quickBatch $ monad x


---------------------------------------

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

instance Monad List where
  (Cons x xs) >>= f = f x <> (xs >>= f)
  _ >>= _ = Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x  <- arbitrary
    xs <- arbitrary
    frequency [(1, pure Nil), (1, pure (Cons x xs))]

instance Eq a => EqProp (List a) where
  (=-=) = eq

checkList :: IO ()
checkList = let x = undefined :: List (String, Bool, Int)
            in quickBatch $ monad x


---------------------------------------

j :: Monad m => m (m a) -> m a
j = (>>= id)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f mx my = my >>= (\y -> (\f' -> f' y) <$> (fmap f mx))

a :: Monad m => m a -> m (a -> b) -> m b
a mx mf = mx >>= (\x -> ($ x) <$> mf)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = foldr (liftM2 (:)) (return []) (f <$> xs)

flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id
