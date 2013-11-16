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

slength :: Stack a -> Integer
slength = toInteger . length
