module Chapter11 where

import Data.List
import Data.Char
import Data.Maybe

-- DaPhone
data DaPhone = DaPhone [Key]

data Key = Key Digit String

one = Key '1' ""
two = Key '2' "ABC"
three = Key '3' "DEF"
four = Key '4' "GHI"
five = Key '5' "JKL"
six = Key '6' "MNO"
seven = Key '7' "PQRS"
eight = Key '8' "TUV"
nine = Key '9' "WXYZ"
zero = Key '0' " +_"
star = Key '*' "^"
hashkey = Key '#' ".,"

daPhone = DaPhone [one, two, three, four, five, six, seven, eight, nine, zero, star, hashkey]

type Digit = Char
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone keys) c = let
  pred (Key d str) = (toUpper c `elem` str) || (c == d)
  key = filter pred keys
  idx (Key d str) = elemIndices (toUpper c) (str ++ [d])
  upper = [('*', 1)]
  lowerpress = do
  k@(Key d _) <- key
  i <- idx k
  return (d, i + 1)
  in if isLower c then lowerpress else upper ++ lowerpress

-- Hutton's Razor
data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x)   = x
eval (Add x y) = eval x + eval y

printExpr :: Expr -> String
printExpr (Lit x)   = show x
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
