module DB where

import qualified Data.Map as Map
import Data.Map (Map, (!))
-- import SQLite () <-- nur zur Veranschaulichung

{- 

Wir wollen eine DSL für Datenbank-Handling, die es ermöglicht,
folgendes Programm zu schreiben:

put "Kaan" 35
x = get "Kaan"
put "Kaan" (x+1)
y = get "Kaan"
return (show (x+y))

(Die DB ist hier ein einfacher Key-Value-Store: String -> Integer)

-}
{-

-- Erster Versuch:

data DBCommand a =
      Put String Integer
    | Get String
    | Return a

-- Dann ist das Programm eine Liste von Commands

type DBProgram a = [DBCommand a]

p1 = [Put "Kaan" 35, Get "Kaan", Put "Kaan" (x+1)]

-- leider funktioniert das nicht, weil wir 
-- - keinen Namen für die Rückgabewerte vergeben können
-- - einzelne Kommandos werden nicht verknüpft

-}

-- Idee: Callback!

data DB a =
      Put String Integer (()      -> DB a)
    | Get String         (Integer -> DB a)
    | Return a

-- Das Programm oben kann man jetzt so schreiben:

p1 = Put "Kaan" 35    (\() ->
     Get "Kaan"       (\x ->
     Put "Kaan" (x+1) (\() -> 
     Get "Kaan"       (\y ->
     Return (show (x+y)))))) 

-- daran gefällt nicht, dass man die einzelnen Zeilen nicht als 
-- "Teilprogramme" aufschreiben kann, weil ineinander verschachtelt
-- Wir schreiben "put"  und "get", als kleinstmögliche Programme:
get key = Get key Return
put key val = Put key val Return

-- damit jetzt möglich, eine Zeile von oben zu schreiben, also ist es möglich,
-- ein Teilprogramm auszulagern
smallProgram = get "Kaan"
smallProgram2 = put "Kaan" 35

-- die sind bis jetzt aber unnütz, weil sie ja sofort terminieren (Return)
-- wir brauchen etwas, das zwei kleine Programme verbindet: 
splice :: DB a -> (a -> DB b) -> DB b
splice (Return a) next             = next a
splice (Get key callback) next     = 
    Get key (\value -> splice (callback value) next)
splice (Put key val callback) next =
    Put key val (\() -> splice (callback ()) next)

-- Programm von oben nur mit "neuer" Schreibweise
p1' = put "Kaan" 35 `splice` (\() ->
      get "Kaan" `splice` (\x -> 
      put "Kaan" (x+1) `splice` (\() -> 
      get "Kaan" `splice` (\y -> 
      Return (show (x+y))))))

-- Hier können jetzt kleine Teilprogramme geschrieben und in größere
-- eingefügt werden 
littleProgram :: DB ()
littleProgram = get "Kaan" `splice` (\x ->
                put "Tom" x)

-- Die Schreibweise ist gewöhnungsbedürftig. Tatsächlich bildet (DB a) eine Monade!
-- dafür noch weitere Instanzen (Functor und Applicative nötig)
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

-- Haskell hat eingebaute Syntax für monadische Programme, die do-Notation
-- Damit sieht unser Programm jetzt aus wie gewünscht!
-- (und wir können das `smallProgram2` bspw. benutzen)
p1'' = 
    do
        smallProgram2
        x <- get "Kaan"
        put "Kaan" (x+1)
        y <- get "Kaan"
        return (show (x+y))

-- Bis jetzt ist das nur eine Beschreibung eines Programm, es ist noch nichts passiert
-- (die eigentliche Datenbank ist noch nicht einmal festgelegt)
-- Jetzt schreiben wir, wie wir im Kontext einer unveränderlichen Map als DB das
-- Programm interpretieren:
runDB :: Map String Integer -> DB a -> (a, Map String Integer)
runDB mp (Return res) = (res, mp)
runDB mp (Get key callback) =
    let age = mp ! key
    in runDB mp (callback age)
runDB mp (Put key val callback) =
    let mp' = Map.insert key val mp
    in runDB mp' (callback ())

{-

-- nur zur Veranschaulichung
-- In tatsächlicher SQLite-Datenbank
runDBSQLite :: Connection -> DB a -> IO a

runDBSQLite conn (Get key callback) =
    do val <- query conn ("SELECT value FROM entries WHERE key = " ++ key ++ ";")
       runDBSQLite conn (callback value)

runDBSQLite conn (Put key value callback) =
    do execute conn "REPLACE INTO entries (key, value) VALUES (" ++ key "," val ++ ");"
       runDBSQLite conn (callback ())
runDBSQLite conn (Return result) = return result

-}