module LiveCoding where

stops = "pbtdkg"
vowels = "aeiou"

allStopVowelStop stops vowels =
  [[s, v, s'] | s <- stops, s == 'p', v <- vowels, s' <- stops]

seekritFunc :: Fractional a => String -> a
seekritFunc x =
  let blah = fromIntegral . length in
  (/) (sum (map blah (words x))) (blah (words x))


myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny = (myOr .) . map

blah = (\g -> ((\f -> myOr . f) . map) g)

myElem :: Eq a => a -> [a] -> Bool
myElem = myAny . (==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []
myMap' :: (a -> b) -> [a] -> [b]
myMap' = flip foldr [] . ((:) .)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = let if' p x = if p x then (x:) else id
             in foldr (if' p) []

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' = myFilter

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = (squish .) . myMap

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp = foldr1 (\x y -> if cmp x y == GT then x else y)
