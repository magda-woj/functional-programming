import Language.Haskell.TH.Syntax (counter)
f :: Integer -> Integer
f n = if even n then n `div` 2 else 3*n + 1

collatzForOne :: Integer -> Integer -> Integer
collatzForOne counter 1 = counter
collatzForOne counter n = collatzForOne (counter + 1) (f n) 

collatz :: Integer -> [Integer]
collatz n = map (collatzForOne 0) [1..n]
