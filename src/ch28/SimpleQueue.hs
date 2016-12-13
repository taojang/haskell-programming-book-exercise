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
pop (Queue [x] [] ) = Just (x, Queue [] [])
pop (Queue enq [] ) = let x : deq  = reverse enq in Just (x, Queue [] deq)
pop (Queue enq deq) = let x : deq' = deq in Just (x, Queue enq deq')

newtype NaiveQ a = NQ { unNQ :: [a] }

npush :: a -> NaiveQ a -> NaiveQ a
npush x (NQ xs) = NQ $ x : xs

npop :: NaiveQ a -> Maybe (a, NaiveQ a)
npop (NQ []) = Nothing
npop (NQ xs) = let (xs', [x]) = splitAt (length xs - 1)  xs in Just (x, NQ xs')


benchQ :: Int -> Queue Int
benchQ i = let go n q
                 | n == 0    = q
                 | odd n     = go (n - 1) (push n q)
                 | otherwise = go (n - 1) (maybe (Queue [] []) snd (pop q))
           in go i (Queue [1..i] [])


benchNQ :: Int -> NaiveQ Int
benchNQ i = let go n q
                  | n == 0    = q
                  | odd n     = go (n - 1) (npush n q)
                  | otherwise = go (n - 1) (maybe (NQ []) snd (npop q))
            in go i (NQ [1..i])

main :: IO ()
main = defaultMain
  [ bench "simple queue" $ whnf benchQ 123456
  , bench "naive queue" $ whnf benchNQ 123456
  ]
