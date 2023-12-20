import System.Win32 (xBUTTON1)
-- zad1 Napisać funkcję dwuargumentową, która przyjmuje liczby x, y oraz zwraca wartość (x^2+2xy)/y^2
-- Wykorzystując napisaną definicję funkcji oraz ustalenie wartości jednym z argumentów zdefiniować funkcję przyjmującą liczbę x oraz zwracającą (25 + 10y)/y^2 
zad1_1 :: Double -> Double -> Double
zad1_1 x y = if y==0 then error "dzielenie przez 0!" else ((x*x) + (2*x*y))/(y*y)

zad1_2:: Double -> Double
zad1_2 y = zad1_1 5 y

-- zad2 Wykorzystując funkcję 
-- sumaWartosci :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Int 
-- sumaWartosci f g x y = (f x) + (g y)
-- zdefiniować funkcję, która przyjmuje liczby x, y oraz zwraca wartość 2x + 3y
sumaWartosci :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
sumaWartosci f g x y = f x + g y

razyDwa :: Int -> Int
razyDwa x = 2*x

razyTrzy :: Int -> Int
razyTrzy x = 3*x

zad2 :: Int -> Int -> Int
zad2 = sumaWartosci razyDwa razyTrzy

-- zad3 Zapisać funkcję
-- ocena :: Double -> String
-- ocena 2.0 = "niezaliczone"
-- ocena 5.0 = "brawo!"
-- ocena x = "wpisane masz " ++ show x
-- stosując dozory (guards) zamiast dopasowania do wzorca.


ocena :: Double -> String
ocena x | x==2.0 = "niezaliczone"
        | x==5.0 = "brawo!"
        | otherwise = "wpisane masz " ++ show x


-- zad4 Napisać funkcję obliczającą s(n, k) - liczbę Stirlinga I rodzaju.

s:: Int -> Int -> Int
s n k   | n==0 && k == 0 = 1
        | k == 0 = 0
        | n==k = 1
        | otherwise = s (n-1) k*(n-1) + s (n-1) (k-1)

-- Napisać stosując rekurencję funkcję o sygnaturze
-- iloczynListy :: [Integer] -> Integer
-- która przyjmuje listę liczba naturalnych i zwraca jej iloczyn.
iloczynListy :: [Integer] -> Integer
iloczynListy list = if list == [] then 1 else head list*iloczynListy (tail list)

-- zad5
-- Napisać funkcję
-- merge :: [Int] -> [Int] -> [Int]
-- łączącą dwie posortowane niemalejąco listy w jedną posortowaną
-- niemalejąco listę. Używając tej funkcji napisać funkcję
-- mergeSort :: [Int] -> [Int]
-- sortującą niemalejąco listę za pomocą algorytmu merge sort

merge :: [Int] -> [Int] -> [Int]
merge left right    | left == [] = right
                    | right == [] = left
                    | head left < head right = head left : merge (tail left) right
                    | otherwise = head right : merge (tail right) left

devide:: [Int] -> ([Int], [Int])
devide list = 
    let n = length list `div` 2 in
        splitAt n list

mergeSort :: [Int] -> [Int]
mergeSort list = 
    if length list == 1 then 
        list 
    else
        let (left, right) = devide list in
            merge (mergeSort left) (mergeSort  right)

-- -- zad6
-- Napisać funkcję
-- czyDoskonala :: Int -> Bool
-- sprawdzającą, czy podana liczba jest doskonała

dzielniki :: (Int, [Int], Int) -> [Int]
dzielniki(n, list, count)
        | count*count > n
            = list
        | count*count==n
            = count:list
        
        | mod n count == 0
            = dzielniki(n, count:(n `div` count):list, count+1)
        
        | otherwise        
            = dzielniki(n, list, count+1)

sumaListy :: [Int] -> Int
sumaListy list = if list == [] then 0 else head list+sumaListy (tail list)

czyDoskonala :: Int -> Bool
czyDoskonala n
        | n == 1 = False
        | n == sumaListy (dzielniki(n, [1], 2)) = True
        |otherwise =False
