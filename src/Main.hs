-- Dla danej liczby naturalnej n ≤ 10000 podaj listę liczb naturalnych z przedziału domkniętego [1, n]
-- które mogą być zapisane przy pomocy sumy dwóch liczb obfitych2
-- . Lista musi zawierać unikalne
-- wartości posortowane w kolejności rosnącej.

import Data.Char (generalCategory)
import Data.List ()
import Text.ParserCombinators.ReadPrec (reset)

obfita :: Integer -> Bool
obfita n = wynik
  where
    dzielnki = [x | x <- [1 .. (n -1)], n `rem` x == 0]
    wynik = sum dzielnki > n

liczbyObfite :: Integer -> [Integer]
liczbyObfite n = liczby_obfite
  where
    liczby_obfite = [x | x <- [0 .. n], obfita x]

-- mozna zapisac przy pomocy 2 liczb ofbitych
czyMoznaLiczbe :: Integer -> Bool
czyMoznaLiczbe n = wynik
  where
    liczby = liczbyObfite n
    wszystkoZeWszystkim = [x + y | x <- liczby, y <- liczby]
    wynik = n `elem` wszystkoZeWszystkim

zad2 :: Integer -> [Integer]
zad2 n = wynik
  where
    wynik = [x | x <- [1 .. n], czyMoznaLiczbe x]
