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
