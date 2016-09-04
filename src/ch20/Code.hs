module Code where

import           Control.Applicative ((<|>))
import           Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (x ==))

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (\x -> (<|> Just x) . (max x <$>)) Nothing

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (\x -> (<|> Just x) . (min x <$>)) Nothing

null' :: (Foldable t) => t a -> Bool
null' = foldr (const . const False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (const succ) 0

toList :: (Foldable t) => t a -> [a]
toList = foldMap pure

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

data Constant a b = Constant a
instance Foldable (Constant a) where
  foldr _ b _ = b

data Two a b = Two a b
instance Foldable (Two a) where
  foldr f x (Two _ y) = f y x
