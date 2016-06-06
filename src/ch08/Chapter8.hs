module Chapter8 where

sumUpTo :: (Eq a, Num a) => a -> a
sumUpTo 1 = 1
sumUpTo x = x + sumUpTo (x - 1)


mult :: (Integral a) => a -> a -> a
mult x 1 = x
mult x y = x + mult x (y - 1)

mc91 :: (Num a, Ord a) => a -> a
mc91 x
  | x > 100   = x - 10
  | otherwise = mc91 $ mc91 x
