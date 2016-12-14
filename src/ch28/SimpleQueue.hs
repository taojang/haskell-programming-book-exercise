module Main where

import Criterion.Main

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Show, Eq)

push :: a -> Queue a -> Queue a
push x (Queue enq deq) = Queue (x : enq) deq

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue []  [] ) = Nothing
pop (Queue enq [] ) = let x : deq  = reverse enq in Just (x, Queue [] deq)
pop (Queue enq (x:xs)) = Just (x, Queue enq xs)

newtype NaiveQ a = NQ { unNQ :: [a] }
  deriving (Show, Eq)

npush :: a -> NaiveQ a -> NaiveQ a
npush x (NQ xs) = NQ $ x : xs

npop :: NaiveQ a -> Maybe (a, NaiveQ a)
npop (NQ []) = Nothing
--npop (NQ xs) = Just (last xs, NQ (init xs))
npop (NQ xs) = let (xs', [x]) = splitAt (length xs - 1)  xs in Just (x, NQ xs')


benchQ :: Int -> Queue Int
benchQ i = let go n q
                 | n == 0    = q
                 | even n    = go (n - 1) (push n q)
                 | otherwise = go (n - 1) (maybe (Queue [] []) snd (pop q))
           in go i (Queue [] [])


benchNQ :: Int -> NaiveQ Int
benchNQ i = let go n q
                  | n  == 0   = q
                  | even n    = go (n - 1) (npush n q)
                  | otherwise = go (n - 1) (maybe (NQ []) snd (npop q))
            in go i (NQ [])

main :: IO ()
main = defaultMain
  [ bench "simple queue" $ whnf benchQ 123456
  , bench "naive queue" $ whnf benchNQ 123456
  ]
