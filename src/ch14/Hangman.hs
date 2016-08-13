module Hangman where

import qualified Data.Set as S
import Test.QuickCheck

-- Puzzle toguess filledInSoFar guessedSoFar
data Puzzle = Puzzle String [Maybe Char] [Char]
  deriving (Eq, Show)

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c:s) where
  zipper guessed wordChar guessChar  =
    if wordChar == guessed
    then Just wordChar
    else guessChar
  newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again"
      return (fillInCharacter puzzle guess)

puzzleGen :: Gen Puzzle
puzzleGen = do
  w           <- arbitrary
  guesslength <- choose (0, S.size . S.fromList $ w)
  guesses     <- guesslength `vectorOf` arbitrary
  let filled = (\x -> if x `elem` guesses then Just x else Nothing) <$> w
  return $ Puzzle w filled guesses

instance Arbitrary Puzzle where
  arbitrary = puzzleGen

prop_fillInChar :: Puzzle -> Char -> Bool
prop_fillInChar p@(Puzzle _ _ s) c = case fillInCharacter p c of
  Puzzle _ _ s' -> s' == c : s

prop_fillInCharReveal :: Puzzle -> Char -> Bool
prop_fillInCharReveal p@(Puzzle w r _) c
  | c `elem` w = case fillInCharacter p c of
                   Puzzle _ revealed _ -> len (c ==) w == len (matching c) revealed where
                     len f = length . filter f
                     matching _ Nothing  = False
                     matching y (Just x) = x == y
  | otherwise  = case fillInCharacter p c of
                   Puzzle _ r' _ -> r' == r
