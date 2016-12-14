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
npop (NQ xs) = Just (last xs, NQ (init xs))


benchQ :: Int -> Queue Int
benchQ i = let go n q
                 | n == 0       = q
                 | mod n 2 == 0 = go (n - 1) (maybe (Queue [] []) snd (pop q))
                 | otherwise    = go (n - 1) (push n q)
           in go i (Queue [] [])


benchNQ :: Int -> NaiveQ Int
benchNQ i = let go n q
                  | n  == 0      = q
                  | mod n 2 == 0 = go (n - 1) (maybe (NQ []) snd (npop q))
                  | otherwise    = go (n - 1) (npush n q)
            in go i (NQ [])

main :: IO ()
main = defaultMain
  [ bench "simple queue" $ whnf benchQ 1234567
  , bench "naive queue" $ whnf benchNQ 1234567
  ]
