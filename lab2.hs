import Language.Haskell.TH (safe)
import Control.Monad (liftM4)
-- import System.Win32 (xBUTTON1)
-- -- zad1
-- Napisać funkcję o sygnaturze
-- jestPunktemStalym :: (Eq a) => a -> (a -> a) -> Bool
-- sprawdzającą, czy punkt podany jako pierwszy argument jest
-- punktem stałym funkcji podanej jako drugi argument

jestPunktemStalym :: Eq a => a -> (a -> a) -> Bool
jestPunktemStalym x f = x == f x

-- for testing
-- funkcja :: Int -> Int
-- funkcja x = x*x

-- jestPunktemStalymfunkcja :: Int -> Bool
-- jestPunktemStalymfunkcja x = jestPunktemStalym x funkcja

-- zad2
-- Napisać funkcję dwuargumentową (możliwie najogólniejszą), która
-- przyjmuje liczby x, y oraz zwraca napis, który informuje nas, czy
-- iloczyn x i y jest większy od ich sumy. W napisie mają pojawić się
-- liczby x i y

sprawdzIloczyn :: (Num a, Show a, Ord a) => a -> a -> String
sprawdzIloczyn a b = "Iloczyn " ++ show a ++ " " ++ show b ++ " jest " ++ (if a*b > a+b then "większy" else "mniejszy") ++ " niż suma"

-- zad3
-- Zdefiniować nieskończoną listę dodatnich rozwiązań układu
-- kongruencji:
-- x ≡ 1 ( mod 6)
-- x ≡ 4 ( mod 7)
-- x ≡ 3 ( mod 8)

zad3 :: [Int]
zad3 = [x | x <- [1..], x `mod` 6 == 1, x `mod` 7 == 4, x `mod` 8 == 3]

-- zad4
-- Zdefiniować funkcję dzielniki :: Int -> [Int] zwracającą
-- listę dzielników liczby z wykorzystaniem funkcji z dzisiejszych
-- materiałów (por. zadanie do pierwszych ćwiczeń z liczbą
-- doskonałą)
dzielniki :: Int -> [Int]
dzielniki n = [x | x <- [1..n], n `mod` x == 0]

-- zad7
-- Napisać funkcję
-- przeksztalcListe :: (Int -> Int) -> [Int] -> [Int]
-- przyjmującą funkcję oraz listę liczb naturalnych, która wykonuje
-- podaną funkcję na każdym elemencie listy (czyli dla funkcji f oraz
-- listy [x1, . . . , xn] mamy otrzymać [f (x1), . . . , f (xn)]).
przeksztalcListe :: (Int -> Int) -> [Int] -> [Int]
przeksztalcListe f xs = [f x | x <- xs]