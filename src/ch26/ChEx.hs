module ChEx where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad
import Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = ReaderT $ Identity . subtract 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \x -> print x >> pure (x + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> print s >> pure (show s, s + 1)



-------

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)
