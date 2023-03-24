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

-- Gleichheit von Tieren überprüfen
eqPet :: Pet -> Pet -> Bool
-- eqPet animal1 animal2 = 
--     case animal1 of
--         Cat -> case animal2 of
--                  Cat -> True
--                  _   -> False
eqPet Cat Cat = True
eqPet Dog Dog = True
eqPet Snake Snake = True
eqPet _ _ = False


instance Eq Pet where
    (==) = eqPet


-- Ein Dillo besteht aus:
-- - Gewicht
-- - lebendig oder tot

data Dillo = MkDillo {dilloWeight :: Integer,
                      dilloAlive :: Bool}
    deriving (Show)