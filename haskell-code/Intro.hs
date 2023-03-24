module Intro where
import Control.Exception (Deadlock)
import Text.XHtml (base)

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

strikeMultiples :: Integral a => a -> [a] -> [a]
strikeMultiples n list = 
    filter (\x -> x `mod` n /= 0) list

sieve [] = []
sieve (x:xs) = x : sieve (strikeMultiples x xs)


divide x y =
    x / y

data Optional a = Result a | Null
    deriving (Show, Eq)

safeDiv :: (Eq a, Fractional a) => a -> a -> Optional a
safeDiv zahl1 zahl2 =
    if zahl2 == 0
    then Null
    else Result (zahl1 / zahl2)

fromResult :: Optional a -> a
fromResult (Result x) = x

-- map :: (a -> b) -> [a] -> [b]

add5 :: Num a => a -> a
add5 x = x + 5

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f Null = Null
optionalMap f (Result x) = Result (f x)

instance Functor Optional where
    fmap :: (a -> b) -> Optional a -> Optional b
    fmap = optionalMap

instance Applicative Optional where
    -- pure  :: a -> Optional a
    -- (<*>) :: Optional (a -> b) -> Optional a -> Optional b
    pure = Result
    Result f <*> Result x = Result (f x)
    _ <*> _ = Null
    
instance Monad Optional where
    -- return :: a -> Optional a
    -- (>>=) :: Optional a -> (a -> Optional b) -> Optional b
    return = pure 
    (Result x) >>= f = f x
    _ >>= _       = Null

da :: (Eq b, Fractional b) => b -> b -> b -> b -> Optional b
da a b c d =

    do 
        x <- safeDiv a b
        y <- safeDiv c d
        return (x+y)

 --   (safeDiv a b) >>= (\x ->
 --   (safeDiv c d) >>= (\y ->
 --   return (x + y)))

listProgram = do
    x <- [1,2 ,3, 4]
    y <- [5,6, 7]
    z <- [2,3]
    return (x,y,z)  

listProgram2 = [(x,y) | x <- [1,2,3,4],y <- [5,6]]