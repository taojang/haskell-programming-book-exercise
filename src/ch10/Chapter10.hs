module Chapter10 where

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

allStopVowelStop :: [(Char, Char, Char)]
allStopVowelStop = [(s, v, s') | s <- stops, v <- vowels, s' <- stops]

allStopVowelStop' :: [(Char, Char, Char)]
allStopVowelStop' = [(s, v, s') | s <- stops, v <- vowels, s' <- stops, s == 'p']

seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x)))
                (length (words x))

seekritFunc' :: (Fractional a) => String -> a
seekritFunc' x =
  (sum (map (fromIntegral. length) $ words x)) / (fromIntegral . length $ words x)

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\y acc ->
                  (x == y) || acc ) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (x ==)

myReverse :: [a] -> [a]
myReverse = foldr (:) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x acc ->
                      if p x then x : acc
                      else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldr1 (\x acc -> if f x acc == GT then x else acc)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldr1 (\x acc -> if f x acc == LT then x else acc)
