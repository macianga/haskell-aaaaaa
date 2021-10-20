-- Dla podanej liczby n podaj, jeśli to możliwe, trójkę pitagorejską a, b, c taką, że a + b + c = n. Dla n =
-- 12 jest nią odpowiednio: (3, 4, 5) ponieważ 32 + 42 = 5
-- 2
-- . Dodatkowo należy narzucić ograniczenie,
-- aby generować tylko pierwotne trójki pitagorejskie. Przykładowo (6, 8, 10) nie jest pierwotną trójką
-- pitagorejską bo wszystkie jej wartości mają wspólny dzielnik równy 2. Jeśli to niemożliwe podaj
-- trójkę pitagorejską dla największego możliwego m gdzie m < n.

import Data.Char (generalCategory)
import Data.List ()
import Text.ParserCombinators.ReadPrec (reset)

generate :: (Integer, Integer) -> (Integer, Integer, Integer)
generate (i, j) = (a, b, c)
  where
    a = i ^ 2 - j ^ 2
    b = 2 * i * j
    c = i ^ 2 + j ^ 2

isMinimal :: (Integer, Integer, Integer) -> Bool
isMinimal (a, b, c) = res
  where
    x = gcd a (gcd b c)
    res = x == 1

somfunc :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
somfunc (i, j, n) = (a, b, c)
  where
    (a, b, c) = generate (i, j)
