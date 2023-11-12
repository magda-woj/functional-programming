import Data.Char (isLower)
import Data.Binary.Get (label)
-- zad1 
-- Napisać używając map i filter funkcję
-- maleLitery :: [String] -> [String]
-- przyjmującą listę napisów i zwracającą listę złożoną tylko i wyłącznie z małych liter alfabetu oryginalnych napisów.
maleLiteryString :: String -> String
maleLiteryString s = [x | x <- ['a'..'z'], elem x s]

maleLitery :: [String] -> [String]
maleLitery l = map maleLiteryString l

-- zad2
-- Napisać używając map i filter funkcję
-- dlugoscPalindromow :: [String] -> Int
-- przyjmującą listę napisów i zwracającą sumę długości tych z nich, które są palindromami.
jestPalindromem :: String -> Bool
jestPalindromem s = reverse s == s

dlugoscPalindromow :: [String] -> Int
dlugoscPalindromow l = sum (map length (filter jestPalindromem l))

-- zad3
-- Napisać używając iterate funkcję
-- fib :: (Integer,Integer) -> [(Integer,Integer)]
-- przyjmującą parę liczb i zwracającą nieskończoną listę, której kolejny element (x′, y′) powstaje z poprzedniego (x, y) w takisposób, że x jest równy y, 
-- a y′jest sumą x i y. Używając tejfunkcji wygenerować nieskończoną listę zawierającą kolejneelementy ciągu Fibonacciego (od 0,1,1,...).
fiboPair :: (Integer, Integer) -> (Integer, Integer)
fiboPair (x, y) = (y, x+y)

fib :: (Integer,Integer) -> [(Integer,Integer)]
fib p = iterate fiboPair p

firsts :: [(a,b)] -> [a]
firsts xs = [x | (x,_) <- xs]

fibonacci :: [Integer]
fibonacci = firsts (fib (0, 1))

-- zad4
-- Zdefiniować funkcję
-- dlugosc :: [a] -> Int
-- działającą jak length, czyli zwracającą długość listy, używając funkcji map i sum (i nie używając length).
dlugosc :: [a] -> Int
dlugosc l = sum (map (\c -> 1) l)

-- -- zad5
-- Napisać funkcję
-- slowaDlugosci :: Char -> Char -> Integer -> [String]
-- przyjmującą dwa znaki oraz liczbę i zwracającą wszystkie słowa podanej długości, których literami są podane znaki. Przykładowo
-- dla ’a’ ’b’ 2 wynikiem powinno być [”aa”,”ab”,”ba”,”bb”].Słowa nie muszą być podane w tej kolejności, ważne, żeby byływszystkie

slowaDlugosci :: Char -> Char -> Int -> [String]
slowaDlugosci x y 1 = [[x],[y]]
slowaDlugosci x y a = (map (x:) (slowaDlugosci x y (a-1))) ++ (map (y:) (slowaDlugosci x y (a-1)))

-- -- zad7
-- Używając zipWith zaimplementować map i zip.
-- Wskazówka: Jeśli listy nie są równej długości, elementy dłuższej
-- ”bez pary” z elementem listy krótszej są pomijane.
myZip :: [a] -> [b] -> [(a, b)]
myZip = zipWith (\x y -> (x,y))

myMap :: (a -> b) -> [a] -> [b]
myMap fun l = zipWith (\ x y -> fun(x)) l [0..] 

-- -- zad8
-- Zaimplementować funkcję o sygnaturze
-- eratosthenes :: Integer -> [Integer]
-- przyjmującą liczbę n i zwracającą listę liczb pierwszych nie
-- większych niż n obliczoną metodą Sita Eratostenesa.

sieve :: Integer -> [Integer] -> [Integer]
sieve p l = if p*p > last l then l
else
    let k = filter (\x-> not (x `mod` p == 0)) l in
        p : sieve (head k) k


eratosthenes :: Integer -> [Integer]
eratosthenes n = sieve 2 [2..n]

