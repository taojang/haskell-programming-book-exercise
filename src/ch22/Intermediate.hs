{-# LANGUAGE InstanceSigs #-}
module Intermediate where
import Data.Char

caps :: [Char] -> [Char]
caps = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = caps . rev

fmapped :: [Char] -> [Char]
fmapped = fmap caps rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> caps <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = caps >>= (\c -> fmap (\r -> (c, r)) rev)
-- tupled' = do
--   c <- caps
--   r <- rev
--   return (c, r)

newtype Reader r a = Reader { runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader ra)= Reader (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> case aRb (ra r) of Reader rb -> rb r

ask' :: Reader a a
ask' = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks' :: (r -> a) -> Reader r a
asks' f = Reader f
