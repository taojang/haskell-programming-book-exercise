module Chapter14 where

import Data.List (sort)
import Test.QuickCheck

half :: Fractional a => a -> a
half x = x / 2

prop_halfIdentity :: (Fractional a, Eq a) => a -> Bool
prop_halfIdentity x = x == ((*2) . half $ x)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered = listOrdered . sort

prop_plusAssociative x y z = x + (y + z) == (x + y) + z
prop_plusCommutative x y = x + y == y + x

prop_quotRem :: (Eq a, Integral a) => a -> NonZero a -> Bool
prop_quotRem x (NonZero y) = (quot x y) * y + (rem x y) == x
prop_divMod :: (Eq a, Integral a) => a -> NonZero a -> Bool
prop_divMod x (NonZero y) = (div x y) * y + (mod x y) == x
prop_divMod' :: (Eq a, Integral a) => a -> a -> Bool
prop_divMod' x y = (div x y) * y + (mod x y) == x

intId :: Integer -> Integer
intId x
  |x > 1 = -1
  |otherwise = x

prop_intId x = x == intId x

prop_reverse xs = (reverse . reverse) xs == id xs

prop_foldrConsConcat xs ys = foldr (:) xs ys == (xs ++ ys)
prop_foldrConcat :: Eq a => [[a]] -> Bool
prop_foldrConcat xs = foldr (++) [] xs == concat xs

prop_showRoundTrip x = (read.show $ x) == x

square x = x * x
squareIdentity x = (square . sqrt $ x) == x

data Fool = Fulse | Frue
  deriving (Eq, Show)

fairFoolGen :: Gen Fool
fairFoolGen = oneof [pure Fulse, pure Frue]

foolGen :: Gen Fool
foolGen = frequency [(2, pure Fulse), (1, pure Frue)]
