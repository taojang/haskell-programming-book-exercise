module Cipher where

import Data.Char

shift :: Int -> Char -> Char
shift n x = chr $ (mod ((ord x) + n) (orda + 26)) + orda
  where orda = ord 'a'

blah = shift 1 'z'

caesar :: Int -> String -> String
caesar n = fmap $ shift n
