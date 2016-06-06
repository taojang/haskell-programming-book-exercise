module Chapter9 where

import Data.Char

allups :: String -> String
allups = filter isUpper

capFirst :: String -> String
capFirst (x : xs) = toUpper x : xs

capAll :: String -> String
capAll []        = []
capAll (x : xs) = toUpper x : capAll xs

capFirst' :: String -> Char
capFirst' = toUpper . head
