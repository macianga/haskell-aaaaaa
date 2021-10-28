-- Dla danej liczby naturalnej n podaj najmniejszą liczbę naturalną m ≥ n z której można utworzyć
-- zbiór A składający się z dwóch rodzajów elementów x i y o mocy |A| = |x|+|y|=m gdzie |x| ≥ |y|.
-- Prawdopodobieństwo wylosowania dwóch elementów x ze zbioru A powinno być równe ½.

-- Przykładowo dla n=20, m=21, |x| = 15 a |y| = 6, prawdopodobieństwo wylosowania dwóch
-- elementów x = P(xx) = (15/21)*(14/20) = 1/2.

-- mocPxx :: Integer -> Integer -> Bool
-- mocPxx m k =if m mod 2 == 1 then if(((m div 2)+k) div m)*(((m div 2)+k-1) div (m-1)) == (1 div 2)then True else False
-- else if(((m+1 div 2)+k) div m)*(((m+1 div 2)+k-1) div (m-1)) == (1 div 2)then True else False
mocPxx :: Integer -> Integer -> Bool
mocPxx m k = m * (m -1) == 2 * k * (k -1)

dowhile :: Integer -> Integer -> Integer
dowhile m k = if (m `div` 2) + k > m -1 && mocPxx m k then m else dowhile m (k + 1)

func :: Integer -> Integer -> Bool
func n k = if mocPxx n k then True else if 2 * k < n -2 then func n (k + 1) else False

result :: Integer -> Integer
result n = if func n (n `div` 2 + n `mod` 2) then n else result (n + 1)

main :: IO ()
main = do
  print "Wrine m"
  n <- readLn
  print (result n)