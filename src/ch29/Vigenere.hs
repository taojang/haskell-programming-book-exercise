{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char
import qualified Data.Map as M
import Data.Traversable (sequenceA)
import Control.Monad.Fix
import Control.Applicative ((<|>), liftA2)
import Control.Monad
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO
import System.Exit

data Args = Encrypt | Decrypt | Key String
  deriving (Show, Eq, Ord)

tokey Encrypt = "e"
tokey Decrypt = "d"
tokey (Key _) = "k"

-- Perform encryption or decryption, depending on f.
crypt f key = map toLetter . zipWith f (cycle key)
  where toLetter = chr . (+) (ord 'A')

-- Encrypt or decrypt one letter.
enc k c = (ord k + ord c) `mod` 26
dec k c = (ord c - ord k) `mod` 26

-- Given a key, encrypt or decrypt an input string.
encrypt = crypt enc
decrypt = crypt dec

-- Convert a string to have only upper case letters.
convert = map toUpper . filter isLetter

flags = [ Option ['e'] [] (NoArg Encrypt) "encryption"
        , Option ['d'] [] (NoArg Decrypt) "decription"
        , Option ['k'] [] (ReqArg Key "KEY") "key"
        ]

main :: IO ()
main = do
  args <- getArgs
  let (opts, _, _) = getOpt Permute flags args
  let opts' = M.fromList $ fmap (\a -> (tokey a, a)) opts
  let lookupOpts = flip M.lookup opts'
  input <- getInput
  run (lookupOpts "d" <|> lookupOpts "e") (lookupOpts "k") input
  where
    getInput = do
      c <- hGetChar stdin
      if c == '\n'
        then return []
        else (c:) <$> getInput
    -- getInput = getLine
    run (Just Encrypt) (Just (Key key)) input = putStrLn $ encrypt key input
    run (Just Decrypt) (Just (Key key)) input = putStrLn $ decrypt key input
    run _              _                _     = die "please use this software correctly."
