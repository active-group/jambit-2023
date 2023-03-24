module DB where

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

