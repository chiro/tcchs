module Stack where

type Stack a = [a]

empty :: Stack a
empty = []

pop :: Stack a -> (a, Stack a)
pop [] = error "stack is empty"
pop (x:xs) = (x,xs)

push :: Stack a -> a -> Stack a
push st x = x:st

push' :: a -> Stack a -> Stack a
push' = flip push

find :: (Eq a) => Stack a -> a -> Bool
find [] _ = False
find (x:xs) t
  | x == t = True
  | otherwise = find xs t

slength :: Stack a -> Integer
slength = toInteger . length
