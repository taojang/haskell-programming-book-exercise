module Chapter5 where

-- Multiple choice
-- 1. c
-- 2. a
-- 3. b
-- 4. c

-- Determine the type
-- 1.
-- a) Num a => a
-- b) Num a => (a, [Char])
-- c) (Integer, [Char])
-- d) Bool
-- e) Int
-- f) Bool
-- 2. Num a => a
-- 3. Num a => a -> a
-- 4. Fractional a => a
-- 5. [Char]

-- Does it compile
-- 1. no. bigNum = (^) 5
-- 2. yes
-- 3. no. c = a 10
-- 4. no. c = 1

-- Type var or type constructor
-- 2.
-- [0] fully polymorphic
-- [1] concrete
-- [2] concrete
-- 3.
-- [0] fully polymorphic
-- [1] constrained Enum b
-- [2] concrete
-- 4.
-- [0] fully poly
-- [1] fully poly
-- [2] concrete


-- Write a type signature
-- 1. [a] -> a
-- 2. Ord a => a -> a -> Bool
-- 3. (a, b) -> b

-- Given a type, write function

i :: a -> a
i = id

c :: a -> b -> a
c = const

c'' :: b -> a -> b
c'' = const

c' :: a -> b -> b
c' _ y = y

r :: [a] -> [a]
r = reverse

co :: (b -> c) -> (a -> b) -> (a -> c)
co = (.)

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' =  ($)

-- Type do know

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f

data A
data B
data C
q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

data X
data Y
data Z
xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f' g' = fst . g' . f'
