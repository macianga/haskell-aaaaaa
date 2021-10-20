-- Dla podanej liczby n podaj, jeśli to możliwe, trójkę pitagorejską a, b, c taką, że a + b + c = n. Dla n =
-- 12 jest nią odpowiednio: (3, 4, 5) ponieważ 3^2 + 4^2 = 5^2
-- . Dodatkowo należy narzucić ograniczenie,
-- aby generować tylko pierwotne trójki pitagorejskie. Przykładowo (6, 8, 10) nie jest pierwotną trójką
-- pitagorejską bo wszystkie jej wartości mają wspólny dzielnik równy 2. Jeśli to niemożliwe podaj
-- trójkę pitagorejską dla największego możliwego m gdzie m < n.

import Data.Char (generalCategory)
import Data.List ()
import Text.ParserCombinators.ReadPrec (reset)

trojki :: Integer -> [[Integer]]
trojki n = [[x, y, z] | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n]]

pit :: [Integer] -> Bool
pit x = wynoik
  where
    wynoik = (x !! 0 * x !! 0 + x !! 1 ^ 2 == x !! 2 ^ 2)

-- krotkaSuma :: (Integer, Integer, Integer) -> Integer
-- krotkaSuma krotka = wynik
--   where
--     wynik = krotka !! 0 + krotka !! 1 + krotka !! 2

trojkipit :: Integer -> [[Integer]]
trojkipit n = trojkiWynik
  where
    asd = [x | x <- trojki n, pit x, sum x == n]
    trojkiWynik =
      if length asd == 0
        then if n > 11 then trojkipit (n -1) else []
        else asd