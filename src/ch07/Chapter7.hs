module Chapter7 where

import Data.Tuple

{--
Intermission:
1. all
2. d
3.
--}
addOneIfOdd = \ n ->
  case odd n of
    True  -> f n
    False -> n
  where f n = n + 1
addFive = (\ x ->
            \ y ->
              (if x > y then y else x) + 5)
myflip f x y = f y x

{--
Intermission:
1. a) (a, b) -> a
   b) String, not safe type
   c) k1 k3
2. f (x, _, y) (x', _, y') = ((x, x'), (y, y'))
--}

{--
Intermission:
1.
--}
functionC x y = case x > y of
  True  -> x
  False -> y

ifEvenAdd2 n = case even n of
  True  -> n + 2
  False -> n

nums x =
  case compare x 0 of
    LT -> -1
    EQ -> 0
    GT -> 1

{--
Intermission:
2. 11
3. 22
4. 21
5. 12
6. 11
7. 21
8. 21
v9. 22
10.31
11.23
--}

{--
Intermission
1. nothing special
2. won't work
3. b
4. [a]
5. [a] -> Bool
6. c
7. Num a => a
8. (Num a, Num b) => a -> b
--}
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  | otherwise = 'F'
  where y = x / 100

{--
Chapter Exercise
1. d
2. b
3. d
4. b
5. a
--}

-- Let's write code
tensDigit :: Integral a => a -> a
tensDigit =  snd . divMod10 . fst . divMod10
  where
    divMod10 = (flip divMod) 10

hunsD ::  Integral a => a -> a
hunsD = (`mod` 10) . (`div` 100)

foldBool :: a -> a -> Bool -> a
foldBool x y z
  | z         = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f = swap . fmap f . swap

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show
