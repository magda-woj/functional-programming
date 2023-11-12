import Language.Haskell.TH (safe)
import Control.Monad (liftM4)
-- import System.Win32 (xBUTTON1)
-- zad1
jestPunktemStalym :: Eq a => a -> (a -> a) -> Bool
jestPunktemStalym x f = x == f x
-- for testing
-- funkcja :: Int -> Int
-- funkcja x = x*x

-- jestPunktemStalymfunkcja :: Int -> Bool
-- jestPunktemStalymfunkcja x = jestPunktemStalym x funkcja

-- zad2
sprawdzIloczyn :: (Num a, Show a, Ord a) => a -> a -> String
sprawdzIloczyn a b = "Iloczyn " ++ show a ++ " " ++ show b ++ " jest " ++ (if a*b > a+b then "większy" else "mniejszy") ++ " niż suma"

-- zad3
zad3 :: [Int]
zad3 = [x | x <- [1..], x `mod` 6 == 1, x `mod` 7 == 4, x `mod` 8 == 3]

-- zad4
dzielniki :: Int -> [Int]
dzielniki n = [x | x <- [1..n], n `mod` x == 0]

-- zad5

f1 :: (Int, Int) -> Bool
f1 (x,y) = x >= 0 && y >= 0

f2 :: (Int, Int) -> Bool
f2 (x,y) = x <= 0 && y >= 0

f3 :: (Int, Int) -> Bool
f3 (x,y) = x <= 0 && y <= 0

f4 :: (Int, Int) -> Bool
f4 (x,y) = x >= 0 && y <= 0


ktoraCwiartka :: [(Int,Int)] -> Int
ktoraCwiartka list = 
    let (l1, l2, l3, l4) = (filter f1 list, filter f2 list, filter f3 list, filter f4 list) in
    max (max (length l1) (length l2)) (max (length l3) (length l4))
    

-- zad6

-- podlisty :: [Integer] -> [[Integer]]

-- zad7
przeksztalcListe :: (Int -> Int) -> [Int] -> [Int]
przeksztalcListe f xs = [f x | x <- xs]