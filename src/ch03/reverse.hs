module Reverse where

main :: IO ()
main = putStrLn $ rvrs "Curry is awesome"

rvrs :: String -> String
rvrs xs = let c = take 5 xs
              i = take 2 $ drop 6 xs
              a = take 7 $ drop 9 xs
          in concat [a, " ", i, " ", c]
