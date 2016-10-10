module LearnParsers where

import Control.Applicative
import Text.Trifecta


stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

oneTwo = char '1' >> char '2' <* eof
oneTwo' = oneTwo >> stop

oneTwoThree = string "123" <|> string "12" <|> string "1"

string' :: String -> Parser String
string' = sequence . fmap char

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParse' :: Parser String -> IO ()
testParse' p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneTwoThree:"
  testParse' oneTwoThree
  pNL "Unit of Success:"
  print $ parseString (integer <* eof) mempty "123"
