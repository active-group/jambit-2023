module Intro where
import Control.Exception (Deadlock)

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

data Liveness = Alive | Dead
    deriving (Show,Eq)

type Weight = Integer
{-
-- Ein Dillo besteht aus:
-- - Gewicht
-- - lebendig oder tot
data Dillo = MkDillo {dilloWeight :: Weight,
                      dilloLiveness :: Liveness}
    deriving (Show,Eq)
-}

dillo1 = MkDillo 20000 Alive
dillo2 = MkDillo 15000 Dead

{-
toBool :: Liveness -> Bool
toBool Alive = True
toBool Dead = False
-}
{-
-- runOverDillo
runOverDillo :: Dillo -> Dillo
-- runOverDillo dillo =
--     MkDillo (dilloWeight dillo) Dead
--runOverDillo (MkDillo weight _) =
--    MkDillo weight Dead
-- runOverDillo dillo@(MkDillo w l) =
--     if dilloLiveness dillo == Dead
--     then dillo
--     else MkDillo w Dead
runOverDillo (MkDillo weight _) =
    MkDillo weight Dead
-}
data Animal =
      MkDillo {dilloWeight :: Weight,
               dilloLiveness :: Liveness}
    | MkParrot String Weight
    deriving (Show)

runOverAnimal (MkParrot _ weight) =
    MkParrot "" weight
runOverAnimal dillo =
    dillo {dilloLiveness = Dead}

listSum :: [Integer] -> Integer
listSum [] = 0
listSum (x:xs) = x + listSum xs

-- LAZY EVALUATION
natsFrom :: Integer -> [Integer]
natsFrom n = n : natsFrom (n+1)

strikeMultiples n list = 
    filter (\x -> x `mod` n /= 0) list