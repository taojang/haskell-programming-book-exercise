module Core_Dump where

discriminatory :: Bool -> Int
discriminatory b =
  case b of
    False -> 0
    True -> 1
