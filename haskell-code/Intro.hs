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


-- Ein Haustier ist eines der folgenen:
-- - Hund
-- - Katze
-- - Schlange
-- -> neuer Typ

data Pet = 
    Cat 
  | Dog
  | Snake 
  deriving (Show)

eqPet :: Pet -> Pet -> Bool
eqPet Cat Cat = True
eqPet Dog Dog = True
eqPet Snake Snake = True
eqPet _ _ = False






