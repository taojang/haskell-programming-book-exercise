{-# LANGUAGE TupleSections #-}
module Chapter23 where

newtype State s a = State { runState :: s -> (a, s) }

-- TODO: add monad instance

get :: State s s
get = State (\x -> (x, x))

put :: s -> State s ()
put s = State $ const ((), s)

exec :: State s a -> s -> s
exec (State sa) = snd . sa

eval :: State s a -> s -> a
eval (State sa) = fst . sa

modify :: (s -> s) -> State s ()
modify = State . ((\s -> ((),s)) .)
