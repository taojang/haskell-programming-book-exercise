module SimpleQueue where

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

newtype NaiveQ a = NQ [a]

npush :: a -> NaiveQ a -> NaiveQ a
npush x (NQ xs) = NQ $ x : xs

npop :: NaiveQ a -> Maybe (a, NaiveQ a)
npop (NQ []) = Nothing
npop (NQ xs) = let (xs', [x]) = splitAt (length xs - 1)  xs in Just (x, NQ xs')
