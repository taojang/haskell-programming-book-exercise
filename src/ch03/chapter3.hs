module Chapter3 where

wow :: String -> String
wow = (++ "!")

fifth :: [a] -> a
fifth = (!! 5)

lastword :: String -> String
lastword = last . words

thirdLetter :: String -> Char
thirdLetter = (!! 3)
