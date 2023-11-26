import System.Win32 (COORD(yPos), xBUTTON1)
-- zad1 Napisać bezpunktowo funkcje równoważne (w sensie zwracanych wartości) poniższym:
-- f list = filter (\x->x>5)) list
-- g list = map (\x->x/5) list
-- Przedstawić kolejne kroki przekształceń od postaci punktowej do
-- bezpunktowej.

f :: (Ord a, Num a) => [a] -> [a]
f list = filter (\x->x>5) list

fBezPkt :: (Ord a, Num a) => [a] -> [a]
fBezPkt = filter (\x->x>5)

g :: Fractional b => [b] -> [b]
g list = map (\x->x/5) list

gBezPkt :: Fractional b => [b] -> [b]
gBezPkt = map (\x->x/5)

-- zad2 Napisać bezpunktowo funkcję o sygnaturze
-- nonZero :: [Int] -> Int
-- obliczającej liczbę niezerowych elementów na liście. Przedstawić kolejne kroki przekształceń od postaci punktowej do bezpunktowej.

nonZero :: [Int] -> Int
-- nonZero list = length (filter (\x-> x/=0) list)
nonZero = length.filter (\x-> x/=0)

-- zad3 Napisać bezpunktowo funkcję równoważną (w sensie zwracanych wartości) poniższej:
-- m x list = map (\y->y*x) list
-- Przedstawić kolejne kroki przekształceń od postaci punktowej do bezpunktowej.

m :: Num b => b -> [b] -> [b]
-- m x list = map (\y->y*x) list
-- m x = map (\y -> y*x)
-- m x = map ((*) x) korzystam z przemienności mnożenia
m = map.(*)


-- zad4 Napisać bezpunktowo funkcję równoważną (w sensie zwracanych wartości) poniższej:
-- d :: [Double] -> Double -> [Double]
-- d list x = map (\y->y/x) list
-- Przedstawić kolejne kroki przekształceń od postaci punktowej do bezpunktowej. 

d :: [Double] -> Double -> [Double]
-- d list x = map (\y->y/x) list
-- d list x = flip map list (\y->y/x) 
-- d list x = flip map list (flip (/) x)  
-- d list = (flip map list).(flip (/))
-- d list = (.) (flip map list) (flip (/))
-- d list = flip (.) (flip (/)) (flip map list)
d = (flip (.) (flip (/))).(flip map)




