module DRegx where

import Data.Function

data Regex =
  Nil | Blank | C Char | Seq Regex Regex | Alt Regex Regex | Rep Regex
  deriving (Eq, Show)

seq' :: Regex -> Regex -> Regex
seq' Nil   _     = Nil
seq' _     Nil   = Nil
seq' Blank p2    = p2
seq' p1    Blank = p1
seq' p1    p2    = Seq p1 p2

alt :: Regex -> Regex -> Regex
alt Nil p2  = p2
alt p1  Nil = p1
alt p1  p2  = Alt p1 p2

-- Kleene *
rep :: Regex -> Regex
rep Nil   = Blank
rep Blank = Blank
rep p     = Rep p

isBlank :: Regex -> Bool
isBlank = (== Blank)

regBlank :: Regex -> Regex
regBlank Blank = Blank
regBlank (Seq p1 p2) = let bothBlank = (&&) `on` (isBlank . regBlank)
                           in if bothBlank p1 p2 then Blank else Nil
regBlank (Alt p1 p2) = let anyBlank = (||) `on` (isBlank . regBlank)
                           in if anyBlank p1 p2 then Blank else Nil
regBlank (Rep _) = Blank
regBlank _ = Nil

regDerivative :: Regex -> Char -> Regex
regDerivative Blank _ = Nil
regDerivative Nil _ = Nil
regDerivative (C c') c = if c == c' then Blank else Nil
regDerivative (Seq p1 p2) c =
  alt (seq' (regDerivative p1 c) p2) (seq' (regBlank p1) (regDerivative p2 c))
regDerivative (Alt p1 p2) c =
  alt (regDerivative p1 c) (regDerivative p2 c)
regDerivative (Rep p) c = seq' (regDerivative p c) (rep p)

regmatch :: Regex -> String -> Bool
regmatch p [] = isBlank p
regmatch p (x : xs) = regmatch (regDerivative p x) xs

main = do
  -- True
  print $ regmatch (Seq (C 'a') (Seq (C 'b') (C 'c'))) "abc"
  -- False
  print $ regmatch (Seq (C 'b') (Seq (C 'b') (C 'c'))) "abc"
