-- Dla podanej liczby n podaj, jeśli to możliwe, trójkę pitagorejską a, b, c taką, że a + b + c = n. Dla n =
-- 12 jest nią odpowiednio: (3, 4, 5) ponieważ 3^2 + 4^2 = 5^2
-- . Dodatkowo należy narzucić ograniczenie,
-- aby generować tylko pierwotne trójki pitagorejskie. Przykładowo (6, 8, 10) nie jest pierwotną trójką
-- pitagorejską bo wszystkie jej wartości mają wspólny dzielnik równy 2. Jeśli to niemożliwe podaj
-- trójkę pitagorejską dla największego możliwego m gdzie m < n.

trojki :: Integer -> [[Integer]]
trojki n = [[x, y, z] | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n]]

czyTrojkaJestPiatgorejska :: [Integer] -> Bool
czyTrojkaJestPiatgorejska x = wynik
  where
    wynik = head x ^ 2 + x !! 1 ^ 2 == x !! 2 ^ 2

czyTrojkaJestPierwotna :: [Integer] -> Bool
czyTrojkaJestPierwotna x = wynik
  where
    a = head x
    b = x !! 1
    c = x !! 1
    wynik = gcd (gcd a b) c == 1

trojkipit :: Integer -> [[Integer]]
trojkipit n = trojkiWynik
  where
    asd = [x | x <- trojki n, czyTrojkaJestPiatgorejska x, czyTrojkaJestPierwotna x, sum x == n]
    trojkiWynik =
      if null asd
        then -- 12 to namniejsza wartosc sumy trojki pitagorejskiej
          if n > 11 then trojkipit (n -1) else []
        else asd

zad1 :: Integer -> [Integer]
zad1 n = wynik
  where
    wynik = head (trojkipit n)