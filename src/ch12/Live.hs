{-# LANGUAGE BangPatterns #-}
module Live where

import Data.Maybe

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x     = Just x

replaceThe :: String -> String
replaceThe x  = case words x of
                  (h : t) -> (fromMaybe "a" . notThe) h ++ (replaceThe . unwords) t
                  []      -> []

vowels = "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel x =
  let countTheRest = countTheBeforeVowel . unwords
  in case words x of
       ("the" : (x' : _) : xs) -> if x' `elem` vowels
                                  then 1 + countTheRest xs
                                  else countTheRest xs
       _                       -> 0

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter (`elem` vowels)

newtype Word' = Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord x = let vc = countVowels x
               cc = (fromIntegral . length) x - vc
           in if vc > cc then Nothing
              else Just (Word' x)

data Nat = Zero | Succ !Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ !x) = let y = natToInteger x in y `seq` 1 + y
  --1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat !x
  | x < 0 = Nothing
  | x == 0 = Just Zero
  | otherwise = Succ <$> integerToNat (x - 1)

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f y = case f <$> y of
                  Nothing -> x
                  Just z  -> z

fromMaybe' :: a -> Maybe a -> a
fromMaybe' x Nothing = x
fromMaybe' _ (Just x) = x

listToMaybe' :: [a] -> Maybe a
listToMaybe' [] = Nothing
listToMaybe' (x:_) = Just x

maybeToList' :: Maybe a -> [a]
maybeToList' Nothing = []
maybeToList' (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [y | x <- xs, y <- maybeToList' x]

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Just x: xs) = (x:) <$> flipMaybe xs
flipMaybe (Nothing : _) = Nothing

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : f x : myIterate f x

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
  Nothing -> []
  Just (y, x') -> y : myUnfoldr f x'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\y -> Just (f y, f y))

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f s = case f s of
  Nothing -> Leaf
  Just (ls, x, rs) -> Node (unfold f ls) x (unfold f rs)
