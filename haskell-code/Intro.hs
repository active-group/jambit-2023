module Intro where

x :: Integer
x = 3

y :: Double
y = 3 / 2 + 5

f :: Integer -> Integer
-- f = (\x -> x + 5)
f x = x + 5

g :: Integer -> Integer 
g x = x * 2

twiceF = f . f

h x y = x + y