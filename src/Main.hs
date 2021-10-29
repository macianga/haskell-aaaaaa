mocPxx :: Integer -> Integer -> Bool
mocPxx m k = m * (m -1) == 2 * k * (k -1)

func :: Integer -> Integer -> Bool
func n k = if mocPxx n k then True else if k < n -1 then func n (k + 1) else False

wynik :: Integer -> Integer
wynik n = if func n (n `div` 2 + n `mod` 2) then n else wynik (n + 1)

main :: IO ()
main = do
  print "Wrine m"
  n <- readLn
  print (wynik n)