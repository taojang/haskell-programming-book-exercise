module Chapter17 where
import Data.Monoid ((<>))
import Control.Applicative (liftA3)

newtype Identity a = Identity a deriving Show

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x

-------------------------------

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

-------------------------------

data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two x0 f) <*> (Two x1 x) = Two (x0 <> x1) (f x)

-------------------------------

vowels :: String
vowels = "aeiou"

stops :: String
stops = "pbtdkg"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
