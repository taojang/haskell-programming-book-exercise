module Chapter12 where

import Data.Maybe

notThe :: String -> Maybe String
notThe x
  | x == "the" = Nothing
  | otherwise  = Just x

replaceThe :: String -> String
replaceThe x  = case words x of
                  (h : t) -> (fromMaybe "a" . notThe) h ++ (replaceThe . unwords) t
                  []      -> []

vowels :: String
vowels = "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel x =
  let countTheRest = countTheBeforeVowel . unwords
  in case words x of
       ("the" : (x' : _) : xs) -> if x' `elem` vowels
                                  then 1 + countTheRest xs
                                  else countTheRest xs
       _                       -> 0

countVowels ::String -> Integer
countVowels x =
  let onlyVowels = filter (`elem` vowels)
      count = fromIntegral . length
  in count [ v | w <- words x, v <- onlyVowels w ]


newtype Word' = Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord "" = Just (Word' "")
mkWord x  =
  let allVowels = filter (`elem` vowels) x
      count     = fromIntegral . length
      numVowels = count allVowels
      allChars  = count $ filter (/= ' ') x
  in
    if numVowels / allChars > 0.5
    then Nothing
    else Just (Word' x)

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0     = Nothing
  | x == 0    = Just Zero
  | otherwise = if x - 1 == 0
                then Just (Succ Zero)
                else Succ <$> integerToNat (x - 1)

isJust' :: Maybe a -> Bool
isJust' (Just _) = True
isJust' _        = False

isNothing' :: Maybe a -> Bool
isNothing' = not . isJust'

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f y = fromMaybe x (f <$> y)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' x Nothing   = x
fromMaybe' _ (Just x') = x'

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = fromMaybe Leaf $
  (\(x0, y, x1) -> Node (unfold f x0) y (unfold f x1)) <$> f x

treeBuild :: Integer -> BinaryTree Integer
treeBuild x
  | x < 1     = Leaf
  | otherwise = let expand y = if y > x - 1 then Nothing else Just (y + 1, y, y + 1)
                    leaf = unfold expand 1
              in Node leaf 0 leaf
