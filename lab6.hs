-- zad 1 Podać przykład działania, wartości początkowej i listy, gdzie wywołanie foldl i foldr da różne wyniki.

--  zad 2 Zaimplementować reverse na liście używając foldl i foldr (po jednej definicji; można założyć, że listy są skończone).

reversel :: [a] -> [a]
reversel l =  foldl (\a b -> b ++ a) [] [[x] | x <- l]

reverser :: [a] -> [a]
reverser l = foldr (\a b -> b ++ a) [] [[x] | x <- l]

-- zad3 Zaimplementować map używając foldl i foldr (po jednej definicji; można założyć, że listy są skończone).


-- zad 4 Zaimplementować używając foldl lub foldr funkcję
-- ktoraCwiartka :: [(Int,Int)] -> Int
-- przyjmującą listę punktów na płaszczyźnie i zwracającą numer ćwiartki, w której jest najwięcej punktów (w przypadku remisu
-- dowolną ćwiartkę, w której jest najwięcej; 

zlicz1Cwiartke :: Int -> (Int,Int) -> Int
zlicz1Cwiartke a (x,y) = if x >= 0 && y >= 0 then a+1 else a

zlicz2Cwiartke :: Int -> (Int,Int) -> Int
zlicz2Cwiartke a (x,y) = if x <= 0 && y >= 0 then a+1 else a

zlicz3Cwiartke :: Int -> (Int,Int) -> Int
zlicz3Cwiartke a (x,y) = if x <= 0 && y <= 0 then a+1 else a

zlicz4Cwiartke :: Int -> (Int,Int) -> Int
zlicz4Cwiartke a (x,y) = if x >= 0 && y <= 0 then a+1 else a

ktoraCwiartka :: [(Int,Int)] -> Int
ktoraCwiartka l = let (p, d, t, c) = (foldl zlicz1Cwiartke 0 l, foldl zlicz2Cwiartke 0 l, foldl zlicz3Cwiartke 0 l, foldl zlicz4Cwiartke 0 l) in
    if p >= d && p >= t && p >= c then 1
    else if d >= t && d >= c then 2
    else if t >= c then 3
    else 4
                  
-- zliczCwiartki :: [(Int,Int)] -> (Int, Int, Int, Int)
-- -- zliczCwiartki l =(foldl zlicz1Cwiartke 0 l, foldl zlicz2Cwiartke 0 l, foldl zlicz3Cwiartke 0 l, foldl zlicz4Cwiartke 0 l)

-- zad6 Napisać funkcję
-- dlaKazdego :: (a -> Bool) -> [a] -> Bool
-- przyjmującą warunek (funkcję jednoargumentową) oraz listę, i zwracającą prawdę, jeśli warunek jest spełniony dla każdego
-- elementu listy oraz fałsz w przeciwnym przypadku. Zdefiniować teżfunkcję
-- istnieje :: (a -> Bool) -> [a] -> Bool
-- przyjmującą warunek (funkcję jednoargumentową) oraz listę, i zwracającą prawdę, jeśli warunek jest spełniony dla przynajmniej
-- elementu listy oraz fałsz w przeciwnym przypadku. W obu przypadkach można założyć, że listy są skończone.

warunekDlaKazdego :: (a -> Bool) -> Bool -> a -> Bool
warunekDlaKazdego f b x = f x && b

dlaKazdego :: (a -> Bool) -> [a] -> Bool
dlaKazdego f l = foldl (warunekDlaKazdego f) True l

warunekIstnieje :: (a -> Bool) -> Bool -> a -> Bool
warunekIstnieje f b x = f x || b

istnieje :: (a -> Bool) -> [a] -> Bool
istnieje f l = foldl (warunekIstnieje f) False l

-- for testing
-- dodatnie :: Int -> Bool
-- dodatnie x = x>0

-- zawsze :: String -> Bool
-- zawsze s = True

-- nigdy :: Int -> Bool
-- nigdy a = False

-- jestPalindromem :: String -> Bool
-- jestPalindromem s = reverse s == s

-- zad7

fibonacci :: [Integer]
fibonacci = 0 : 1 : (zipWith (+) fibonacci (tail fibonacci))