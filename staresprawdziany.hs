-- sprawdzian 2013/2014

-- Zadanie 1. Napisac funkcje oddbins n, generujaca liste wszystkich ciagów binarnych o długosci n, w których liczba jedynek jest nieparzysta. 
-- Ciagi reprezentujemy w postaci list, Kolejnosc ciagów w liscie nie ma znaczenia

ciagiDlugosci :: Int -> [[Int]]
ciagiDlugosci 1 = [[1],[0]]
ciagiDlugosci a = (map (1:) (ciagiDlugosci (a-1))) ++ (map (0:) (ciagiDlugosci (a-1)))


oddbins :: Int -> [[Int]]
oddbins n = filter (\l -> odd(sum l)) (ciagiDlugosci n)

-- Zadanie 2. Napisac funkcje 
-- diffsums :: [[Int]] → [[Int]]
--  która z wejsciowej listy usuwa listy o powtarzajacej sie sumie. Na przykład diffsums [[1, 2], [3, 4, 5], [3], [], [7, 5]] = [[1, 2], [3, 4, 5], []] lub [[7, 5], [3], []] itp.
--  W rozwiazaniu nalezy uzyc funkcji foldl lub foldr. Kolejnosc w liscie wynikowej nie ma znaczenia, ale kolejnosc w blokach ma zostac zachowana

jestNaLiscieSum :: [Int] -> [Int] -> Bool
jestNaLiscieSum sumy l = elem (sum l) sumy

-- diffsums :: [[Int]] -> [[Int]]
-- diffsums l

-- sprawdzian 2015/2016

-- Zadanie 1. Każde słowo złożone z liter a i b przekształcamy idąc od lewej według następujących reguł: ab → a, ba → b, bb → a, aa → aaa 
-- (jeśli słowo ma nieparzystą długość, ostatnią literę przepisujemy). 
-- Napisać funkcję dlugosc, zwracającą liczbę iteracji powyższych reguł, które prowadzą do słowa złożonego
-- z samych liter a lub słowa długości mniejszej niż 2.

-- 2017 zad 1
dzielniki :: Integer -> [Integer]
dzielniki n = if n >= 0 then [x | x <- [1..n], n `mod` x == 0] else [x | x <- [1..(-n)], n `mod` x == 0]

rd :: Integer -> [Integer]
rd n = [k | k <- [-n..n*n], sum (dzielniki k) == n+k]