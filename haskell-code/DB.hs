module DB where

import qualified Data.Map as Map
import Data.Map (Map, (!))
import SQLite ()
import Language.Haskell.TH (valD)

{- 
DB: Key-Value-Store: String -> Integer

put "Kaan" 35
x = get "Kaan"
put "Kaan" (x+1)
y = get "Kaan"
return (show (x+y))

-}
{-
data DBCommand a =
      Put String Integer
    | Get String
    | Return a

type DBProgram a = [DBCommand a]

p1 = [Put "Kaan" 35, Get "Kaan", Put "Kaan" (x+1)]
-- leider doch nicht möglich, weil wir 
-- - keinen namen vergeben können
-- - einzelne kommandos nicht verknüpft

-}

-- Callback!

data DB a =
      Put String Integer (()      -> DB a)
    | Get String         (Integer -> DB a)
    | Return a

p1 = Put "Kaan" 35    (\() ->
     Get "Kaan"       (\x ->
     Put "Kaan" (x+1) (\() -> 
     Get "Kaan"       (\y ->
     Return (show (x+y)))))) 

get key = Get key Return

put key val = Put key val Return

smallProgram = get "Kaan"

smallProgram2 = put "Kaan" 35

splice :: DB a -> (a -> DB b) -> DB b
splice (Return a) next             = next a
splice (Get key callback) next     = 
    Get key (\value -> splice (callback value) next)
splice (Put key val callback) next =
    Put key val (\() -> splice (callback ()) next)

p1' = put "Kaan" 35 `splice` (\() ->
      get "Kaan" `splice` (\x -> 
      put "Kaan" (x+1) `splice` (\() -> 
      get "Kaan" `splice` (\y -> 
      Return (show (x+y))))))



littleProgram :: DB ()
littleProgram = get "Kaan" `splice` (\x ->
                put "Tom" x)

instance Functor DB where
    -- fmap :: (a -> b) -> DB a -> DB b
    fmap f (Return res) = Return (f res)
    fmap f (Get key callback) =
        Get key (\x -> fmap f (callback x))
    fmap f (Put key val callback) =
        Put key val (\() -> fmap f (callback ()))


instance Applicative DB where
    pure = Return
    (<*>) ff fa = ff >>= (\ f -> fmap f fa)


instance Monad DB where
    (>>=) = splice

p1'' = 
    do
        smallProgram2
        x <- get "Kaan"
        put "Kaan" (x+1)
        y <- get "Kaan"
        return (show (x+y))

runDB :: Map String Integer -> DB a -> (a, Map String Integer)
runDB mp (Return res) = (res, mp)
runDB mp (Get key callback) =
    let age = mp ! key
    in runDB mp (callback age)
runDB mp (Put key val callback) =
    let mp' = Map.insert key val mp
    in runDB mp' (callback ())

runDBSQLite :: Connection -> DB a -> IO a

putToSQLite :: String -> Int -> IO ()
putToSQlite key val = 
    do 
        SQL.insert key val


