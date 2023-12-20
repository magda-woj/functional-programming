{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import Control.Arrow
import System.Win32.Automation (xBUTTON1)
import Language.Haskell.TH (RuleBndr(TypedRuleVar))
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use bimap" #-}

-- sprawdzian 2013/2014

-- Zadanie 1. Napisac funkcje oddbins n, generujaca liste wszystkich ciagów binarnych o długosci n, w których liczba jedynek jest nieparzysta. 
-- Ciagi reprezentujemy w postaci list, Kolejnosc ciagów w liscie nie ma znaczenia

-- zad 1 2013
ciagiDlugosci :: Int -> [[Int]]
ciagiDlugosci 1 = [[1],[0]]
ciagiDlugosci a = map (1:) (ciagiDlugosci (a-1)) ++ map (0:) (ciagiDlugosci (a-1))

oddbins :: Int -> [[Int]]
oddbins n = filter (odd . sum) (ciagiDlugosci n)

-- Zadanie 2 2013. Napisac funkcje 
-- diffsums :: [[Int]] → [[Int]]
--  która z wejsciowej listy usuwa listy o powtarzajacej sie sumie. Na przykład diffsums [[1, 2], [3, 4, 5], [3], [], [7, 5]] = [[1, 2], [3, 4, 5], []] lub [[7, 5], [3], []] itp.
--  W rozwiazaniu nalezy uzyc funkcji foldl lub foldr. Kolejnosc w liscie wynikowej nie ma znaczenia, ale kolejnosc w blokach ma zostac zachowana

-- zad2 2013
uniqueSum :: [[Int]] -> [Int] -> [[Int]]
uniqueSum lists l = let sums = map sum lists in
  if sum l `elem` sums
    then lists
    else l : lists

diffsums :: [[Int]] -> [[Int]]
diffsums = foldl uniqueSum []

-- Zadanie 3 2013. Napisac bezpunktowo funkcje compref, podajaca długosc najdłuzszego zgodnego odcinka poczatkowego dwóch list, 
-- oraz podac najogólniejsza mozliwa sygnature.
compref :: Eq a => [a] -> [a] -> Int
-- compref p q = length (filter (uncurry (==)) (zip p q))
-- compref p q = length$(filter (uncurry (==))) ((zip p) q)
-- compref p q = (length.(filter (uncurry (==)))) ((zip p) q)
-- compref p q = ((length.(filter (uncurry (==)))).(zip p)) q
-- compref p = (length.(filter (uncurry (==)))).(zip p)
-- compref p = (.) (length.(filter (uncurry (==)))) (zip p)
-- compref p = ( ((.) (length.(filter (uncurry (==))))).zip ) p
compref = ((length.(filter (uncurry (==)))) .).zip

-- sprawdzian 2014/2015
-- Zadanie 1. Napisac bezpunktowo funkcje numocc, która zlicza wystapienia wskazanego elementu w podanych listach, 
-- tzn. numocc x [l1, l2, ...ln] = [a1, a2, ..., an], gdzie ai to liczba wystapien x w liscie li.
-- Podac najogólniejsza mozliwa sygnature

-- 2014 zad1
countAppearances :: Eq a => a -> [a] -> Int
-- countAppearances x l = length (filter (==x) l)
-- countAppearances x = length.filter (==x)
countAppearances = (length.).(filter.(==))

numocc :: Eq a => a -> [[a]] -> [Int]
-- numocc x list = map (countAppearances x) list
numocc = map.countAppearances

-- jakies jebane drzewo operacji co Kuba Kural robil
-- 2014 zad2
data Op = Add | Mul | Neg
data CT a = EmptyLeaf | Leaf a | Join (CT a) Op (CT a)
wf :: CT a -> Bool
wf EmptyLeaf = False
wf (Leaf _) = True
wf (Join EmptyLeaf Neg EmptyLeaf) = False
wf (Join EmptyLeaf Neg _) = True
wf (Join _ Neg EmptyLeaf) = True
wf (Join _ Neg _) = False
wf (Join left _ right) = wf left && wf right
eval :: Num a => CT a -> a
eval (Leaf x) = x
eval (Join left Add right) = eval left + eval right
eval (Join left Mul right) = eval left * eval right
eval (Join left Neg EmptyLeaf) = - eval left
eval (Join EmptyLeaf Neg right) = - eval right

-- Zadanie 3. Napisac bezpunktowo funkcje h, która zwraca elementy podanej listy znajdujace sie na pozycjach o numerach parzystych, 
-- w takiej kolejnosci, w jakiej wystepuja w oryginalnej liscie.

-- 2014 zad3
h :: [a] -> [a]
-- h (x:y:xs) = x : h xs
-- h xs = xs
-- h xs = map snd (filter (odd.fst) (zip [1..] xs))
h = map snd.(filter (odd.fst).zip [1..])

-- sprawdzian 2015/2016

-- Zadanie 1. Każde słowo złożone z liter a i b przekształcamy idąc od lewej według następujących reguł: 
-- ab → a, ba → b, bb → a, aa → aaa 
-- (jeśli słowo ma nieparzystą długość, ostatnią literę przepisujemy). Napisać funkcję dlugosc,
-- zwracającą liczbę iteracji powyższych reguł, które prowadzą do słowa złożonego z samych liter a lub słowa długości mniejszej niż 2

-- zad1 2015
przeksztalc :: String -> String
przeksztalc "ab" = "a"
przeksztalc "ba" = "b"
przeksztalc "bb" = "a"
przeksztalc "aa" = "aaa"
przeksztalc s = if length s == 1 then s else
    przeksztalc ([(head s), (s!!1)]) ++ przeksztalc (tail (tail s))

dlugoscZlicz :: String -> Int -> Int
dlugoscZlicz s count = if length s < 2 || notElem 'b' s then count
  else dlugoscZlicz (przeksztalc s) (count+1)

dlugosc :: String -> Int
dlugosc s = dlugoscZlicz s 0

-- Zadanie 2. (a) Dla dwóch liczb całkowitych a, b > 1 oznaczamy przez val(a, b) największą potęgę liczby b, która dzieli a, 
-- np. val(56, 2) = 3, bo 2^3 dzieli 56, ale 2^4 nie dzieli 56; podobnie val(56, 3) = 0, bo 3^1 nie dzieli 56. Napisać funkcję:
-- val :: Integer -> Integer -> Integer
-- która dla podanych a i b oblicza val(a, b)

-- zad 2a 2015
val :: Integer -> Integer -> Integer
val a b = if a `mod` b /= 0 then 0 else (val (a `div` b) b)+1

-- (b) Za pomocą powyższej funkcji val napisać funkcję:
-- g :: Integer -> Integer -> [Integer]
-- która dla podanych k > 1 oraz v >= 0 zwraca listę (nieskończoną, w dowolnej kolejności) liczb naturalnych n > 1, takich że 
-- val(n, k) = v. Przykładowo: g 2 0 = [3,5,7,9,...] g 3 1 = [3,6,12,15,21,...]

-- zad 2b 2015
g :: Integer -> Integer -> [Integer]
g k v = [n | n  <- [2..], val n k == v]

-- Zadanie 3. Kłosem nazywamy strukturę danych o funkcjonalności przypominającej listę, która umożliwia dokładanie elementów na początek i na koniec
-- w czasie stałym, a ponadto odczytanie elementów po kolei. Stworzyć typ Klos aprzechowujący elementy typu a. Zdefiniować funkcje:
-- wnpk :: Klos a -> a -> Klos a
-- wnkk :: Klos a -> a -> Klos a
-- k2list :: Klos a -> [a]
-- Funkcje mają, odpowiednio, wstawiać element na początek i na koniec kłosa oraz zamieniać kłos na listę. W ostatniej funkcji nie nakładamy 
-- ograniczenia na złożoność.

-- zad 3/2015
data Klos a = Klos {
                beg :: [a],
                end :: [a]
                } deriving Show
 
klosInst = Klos {beg = [1..4], end = [4,3,2,1] }
 
wnpk :: Klos a -> a -> Klos a
wnpk (Klos beg end) elem = Klos (elem:beg) end
 
wnkk :: Klos a -> a -> Klos a
wnkk (Klos beg end) elem = Klos beg (elem:end)
 
k2list :: Klos a -> [a]
k2list (Klos beg end) = beg ++ reverse end
 
wnpkDemo = k2list (wnpk klosInst 1)
wnkkDemo = k2list (wnkk klosInst 4)

-- sprawdzian 2016/2017
-- 2016 zad1
divi :: Integer -> [Integer]
divi 0 = []
divi n = divi (n `div` 10) ++ [n `mod` 10]

pownum :: Integer -> [Integer]
pownum n = [x | x <- [1..], sum (map (\x -> x^n) (divi x)) == x]

-- Zadanie 2. Napisać funkcję ps, która dla podanej listy zwraca listę zawierającąwszystkie jej prefiksy — w kolejności od najkrótszego 
-- (jednoelementowego) do najdłuższego (cała lista) — a następnie kolejno coraz krótsze sufiksy tej listy. Przykładowo:
-- ps "Test" = ["T", "Te", "Tes", "Test", "est", "st", "t"]
-- ps [3,5,2] = [[3], [3,5], [3,5,2], [5,2], [2]]
-- Dla pustej listy wynik może być dowolny. W rozwiązaniu należy w istotny sposób użyć funkcji foldl lub foldr.

-- 2016 zad2
prefix :: Show a => [[a]] -> a -> [[a]]
prefix [] e = [[e]]
prefix prefixes e = prefixes ++ [last prefixes ++ [e]]

sufix :: Show a => [a] -> [[a]] -> a -> [[a]]
sufix ogList [] e = [ogList]
sufix ogList sufixes e = sufixes ++ [tail (last sufixes)]

ps :: Show a => [a] -> [[a]]
ps [] = []
ps list = foldl prefix [] list ++ foldl (sufix (tail list)) [] (tail list)

-- Zadanie 3. Rododendronem nazywamy drzewo, w którym każdy wierzchołek może mieć dowolną liczbę potomków (być może zero). Rododendron nie może
-- być pusty. Stworzyć typ Rd a, przechowujący elementy typu a w rododendronie, i zdefiniować funkcje:
-- el :: Eq a => Rd a -> a -> Bool
-- subst :: Eq a => a -> a -> Rd a -> Rd a
-- rd2list :: Rd a -> [a]
-- Funkcje mają, odpowiednio: sprawdzać czy podany element należy do rododendronu, zamieniać wszystkie wystąpienia pierwszego podanego elementu 
-- na drugi oraz zamieniać rododendron na listę zgodnie z porządkiem preorder.

-- 2016 zad 3
data Rd a = RdNode a [Rd a]

rodo :: Rd Integer
rodo = RdNode 10 [RdNode 8 [RdNode 3 []], RdNode 6 [], RdNode 4 [RdNode 6 [], RdNode 1 []]]

-- el :: Eq a => Rd a -> a -> Bool
-- el (RdNode a []) e = a == e
-- el (RdNode a subRds) e = a == e || (elem True (map (flip el e) subRds))

subst :: Eq a => a -> a -> Rd a -> Rd a
subst e f (RdNode a []) = if e == a then (RdNode f []) else (RdNode a [])
subst e f (RdNode a subRds) = if e == a then (RdNode f (map (subst e f) subRds)) else (RdNode a (map (subst e f) subRds))

rd2list :: Rd a -> [a]
rd2list (RdNode a []) = [a]
rd2list (RdNode a subRds) = a : (foldl (++) [] (map rd2list subRds))

-- sprawdzian 2017/2018
-- 2017 zad 1
dzielniki :: Integer -> [Integer]
dzielniki n = if n >= 0 then [x | x <- [1..n], n `mod` x == 0] else [x | x <- [1..(-n)], n `mod` x == 0]

rd :: Integer -> [Integer]
rd n = [k | k <- [-n..n*n], sum (dzielniki k) == n+k]

-- Zadanie 2. Napisać funkcję repl :: Eq a ⇒ [a] → [(a, a)] → [a], która dla danej listy l oraz listy par dokonuje zamian elementów w l 
-- w taki sposób, że każde wystąpienie pierwszego elementu pewnej pary zostaje zamienione na drugi element tej pary, np.
-- repl [1,2,3,1,2] [(2,4)] = [1,4,3,1,4]
-- repl "alamakota" [(’a’,’u’), (’o’,’e’)] = "ulumuketu"
-- W rozwiązaniu należy w istotny sposób użyć funkcji foldl lub foldr. Uwaga: można założyć, że wszystkie elementy występujące w parach są różne, 
-- tzn. lista k par (a1, b1), ...,(ak, bk) zawiera 2k różnych elementów a1, ..., ak, b1, ..., bk.

-- zad 2 2017
addSnd :: Eq a => [(a, a)] -> a -> [a] -> [a]
addSnd pairs e list = if e `elem` map fst pairs then [snd p | p <- pairs, fst p == e ] ++ list else e : list

repl :: Eq a => [a] -> [(a, a)] -> [a]
repl list pairs = foldr (addSnd pairs) [] list

-- Zadanie 3. Słabym drzewem binarnym nazywamy strukturę, w której każdy wierzchołek zawiera pewną wartość, a ponadto ma 0, 1 lub 2 wierzchołki 
-- potomne. W tym ostatnim przypadku kolejność potomków nie jest rozróżniana, tzn. drzewa różniące się jedynie kolejnością potomków uznajemy za równe.
-- Słabe drzewo binarne nie może być puste. Stworzyć typ Sdb a, przechowujący elementy typu a w słabym drzewie binarnym, i zdefiniować funkcje:
-- el :: Eq a => Sdb a -> a -> Bool
-- eq :: Eq a => Sdb a -> Sdb a -> Bool
-- sdb2list :: Sdb a -> [a]
-- Funkcje mają, odpowiednio: sprawdzać czy podany element należy do drzewa, sprawdzać czy podane drzewa są równe oraz zamieniać drzewo na listę 
-- przeszukując je wszerz (kolejność przeglądania potomków może być dowolna).

-- zad 3/2017
-- data Sdb a = Node0 a | Node1 a (Sdb a) | Node2 a (Sdb a) (Sdb a)

-- -- checking if element is in the tree
-- el :: Eq a => Sdb a -> a -> Bool
-- el (Node0 root) elem = root == elem
-- el (Node1 root subtree) elem = root == elem || el subtree elem
-- el (Node2 root subtreel subtreer) elem = elem == root || el subtreel elem || el subtreer elem

-- -- equality of trees
-- eq :: Eq a => Sdb a -> Sdb a -> Bool
-- -- eq (Node0 {}) (Node1 {}) = False
-- -- eq (Node1 {}) (Node0 {}) = False
-- -- eq (Node0 {}) (Node2 {}) = False
-- -- eq (Node2 {}) (Node0 {}) = False
-- -- eq (Node1 {}) (Node2 {}) = False
-- -- eq (Node2 {}) (Node1 {}) = False
-- eq (Node0 root1) (Node0 root2) = root1==root2
-- eq (Node1 r1 s1) (Node1 r2 s2) = r1 == r2 && eq s1 s2
-- eq (Node2 r1 sl1 sr1) (Node2 r2 sl2 sr2) = (r1 == r2) && ((eq sl1 sl2 && eq sr1 sr2) || (eq sl1 sr2 && eq sr1 sl2))
-- eq _ _ = False

-- parse to list
-- sdb2list :: Sdb a -> [a]
-- sdb2list (Node0 root) = [root]
-- sdb2list (Node1 root subtree) = root : sdb2list subtree
-- sdb2list (Node2 root subtreel subtreer) = root : sdb2list subtreel ++ sdb2list subtreer

-- addRoot2List :: Sdb a -> [a] -> [a]
-- addRoot2List (Node0 root) list = root : list
-- addRoot2List (Node1 root subtree) = root : list
-- addRoot2

-- sdb2listQueue :: [Sdb a] -> ([Sdb a], [a])
-- sdb2listQueue [trees] = 


  -- zad 3
-- data Tree a = Empty | Node a (Tree a) (Tree a)
-- data Sdb a = Value a (Tree a) (Tree a)

-- el :: Eq a => Sdb a -> a -> Bool
-- el (Value a b c) x
--     | a == x = True
--     | otherwise = elTree b x || elTree c x

-- elTree :: Eq a => Tree a -> a -> Bool
-- elTree (Node a b c) x
--     | a == x = True
--     | otherwise = elTree b x || elTree c x
-- elTree Empty _ = False

-- eq :: Eq a => Sdb a -> Sdb a -> Bool
-- eq (Value a1 b1 c1) (Value a2 b2 c2) = a1 == a2 && ((eqTree b1 b2 && eqTree c1 c2) || (eqTree b1 c2 && eqTree c1 b2))

-- eqTree :: Eq a => Tree a -> Tree a -> Bool
-- eqTree Empty Empty = True
-- eqTree (Node a1 b1 c1) (Node a2 b2 c2) = a1 == a2 && ((eqTree b1 b2 && eqTree c1 c2) || (eqTree b1 c2 && eqTree c1 b2))
-- eqTree _ _ = False

-- sdb2list :: Sdb a -> [a]
-- sdb2list (Value a b c) = a:treeBFS [b, c]

-- treeBFS :: [Tree a] -> [a]
-- treeBFS [] = []
-- treeBFS xs = [a | (Node a _ _) <- xs] ++ treeBFS ([b | (Node _ b _) <- xs] ++ [c | (Node _ _ c) <- xs])

-- sprawdzian 2018/2019
-- Zadanie 1. Rozwazmy ciagi (an),(bn), które spełniaja nastepujaca zaleznosc rekurencyjna
-- a_n = (n-1)b_(n-1) - 3a_(n-1)
-- b_n = 3b_(n-1) + (n-1)^2 * a_(n-1) - (n - 1)^2
-- Dodatkowo wiemy, ze a0 = b0 = 1. Napisac funkcje seqIndex m, która zwraca najmniejsze k takie, ze a_0+a_1+...+a_k >= m. 
-- Na przykład seqIndex 100 = 4, seqIndex 1000000 = 8

-- zad1 2018
a :: Int -> Int
a 0 = 1
a n = (n-1)*(b (n-1)) - 3*(a (n-1))

b :: Int -> Int
b 0 = 1
b n = 3*(b (n-1)) + (n-1)*(n-1)*(a (n-1)) - (n-1)*(n-1)

an :: [Int]
an = map a [0 ..]
 
seqIndex :: Int -> Int
seqIndex m = length (takeWhile (\el -> sum (take el an) < m) [1 ..])

-- -- Zadanie 2. Rozwa˙zmy typ danych przechowuj ˛acy cz˛e´sciowe wyra˙zenia, tzn. wyra˙zenia, które zawieraj ˛a operacje dodawania (Add), mno˙zenia (Mul) i odejmowania (Sub) oraz warto´s´c P, która oznacza, i˙z konkretny argument nie jest jeszcze znany. Argumenty do operacji arytmetycznych przechowujemy za pomoc ˛a Value. Napisa´c funkcj˛e eq :: (Eq a) → Expr a → Expr a → Bool,
-- która zwraca True, je´sli wyra˙zenia s ˛a takie same, i False w przeciwnym wypadku. Przyjmujemy, ˙ze dwa wyra˙zenia s ˛a takie same, je´sli jedno mo˙zna otrzyma´c z drugiego przez
-- zamiany P na dowolne inne wyra˙zenia. Na przykład


-- Zadanie 3. Napisac funkcje cykl, która dla podanej niepustej listy zwraca liste jej wszystkich przesuniec cyklicznych, w dowolnej kolejnosci. 
-- Podac najogólniejsza mozliwa sygnature. Funkcja ma w nietrywialny sposób korzystac z foldl lub foldr, przy czym fold musi stanowic najbardziej 
-- zewnetrzna czesc definicji, Przykładowo, wywołanie cykl [1,2,3] powinno zwrócic [[1,2,3], [2,3,1], [3,1,2]] lub dowolna permutacje takiej listy.

-- zad3 2018

dodajCykl :: a -> [[a]] -> [[a]]
dodajCykl e cykle = (e : (init (head cykle))) : cykle

cykl :: [a] -> [[a]]
cykl xs = foldr dodajCykl [xs] (tail xs)

-- sprawdzian 2019/2020
-- 2019 zad 1
sevens :: Int -> [Int]
sevens n = take n $ filter (\x -> rozklad x == 7) [1..]
rozklad :: Int -> Int
rozklad n
  | n < 10 = n
  | otherwise = rozklad (sumaCyfr n)
sumaCyfr :: Int -> Int
sumaCyfr n
  | n < 10 = n
  | otherwise = (n `mod` 10) + sumaCyfr (n `div` 10)


-- zad 2 zbalansowany palindrom
palidromeGenerator :: Int -> [String]
palidromeGenerator n 
    | n<2 = [""]
    | otherwise = map (\x -> "a" ++ x ++ "a") (palidromeGenerator (n-2)) ++ map (\x -> "b" ++ x ++ "b") (palidromeGenerator (n-2))

count :: Char -> [Char] -> Int
count n = foldl (\acc x -> if n == x then acc + 1 else acc) 0

balancedPalidrome :: Int -> [String]
balancedPalidrome n
    | n `mod` 4 > 0 = [""]
    | otherwise = filter (\x -> count 'a' x == count 'b' x) (palidromeGenerator n)

-- zad 3 milorzab O(1) itp
data Mb a = Mb [a] [a]

dnp :: Mb a -> a -> Mb a
dnp (Mb xs ys) x = Mb (x:xs) ys

dnk :: Mb a -> a -> Mb a
dnk (Mb xs ys) y = Mb xs (y:ys)

mb2list :: Mb a -> [a]
mb2list (Mb xs ys) = xs ++ reverse ys

ull :: Mb a -> Mb a
ull (Mb _ ys) = Mb [] ys

ulr :: Mb a -> Mb a
ulr (Mb xs _) = Mb xs []
-- sprawdzian 2020/2021
-- Zadanie 1. Niech type Point = (Double, Double). Napisac funkcje
-- minDist :: [Point] -> (Point, Point, Double),
-- która zwróci pare punktów najblizszych sobie na liscie podanej jako argument, wraz z odległoscia miedzy nimi. W przypadku wiekszej 
-- liczby mozliwych rozwiazan nalezy wybrac dowolne; takze kolejnosc elementów w parze wynikowej nie ma znaczenia.
-- W rozwiazaniu mozna uzyc faktu, iz (Double, Double) jest w klasie Eq

-- type Point = (Double, Double)

-- minDist :: [Point] -> (Point, Point, Double)
-- minDist xs = 

-- Zadanie 2. Rozwazmy nastepujaca definicje drzewa: data Tree a = Empty | Node a (Tree a) (Tree a).
-- Napisac funkcje findPath :: Eq a => a -> Tree a -> [a], która zwraca sciezke (w postaci listy) od korzenia drzewa do elementu podanego jako
-- pierwszy argument. Prosze załozyc, ze w drzewie nie ma powtarzajacych sie elementów. Na przykład dla
-- t=Node 10 (Node 5 (Node 4 Empty Empty) (Node 6 Empty Empty)) (Node 20 Empty Empty)
-- mamy findPath 6 t = [10,5,6] oraz findPath 7 t = [].

-- 2020 zad 2
data Tree a = Empty | Node a (Tree a) (Tree a)

findPath :: Eq a => a -> Tree a -> [a]
findPath e Empty = []
findPath e (Node a left right) | a == e = [e]
                               | leftPath /= [] = a : leftPath
                               | rightPath /= [] = a: rightPath
                               | otherwise = []
                               where 
                                leftPath = findPath e left
                                rightPath = findPath e right

t :: Tree Integer                          
t=Node 10 (Node 5 (Node 4 Empty Empty) (Node 6 Empty Empty)) (Node 20 Empty Empty)


-- Zadanie 3. W permutacji liczb od 1 do n pozycje k nazywamy zamykajaca, jezeli wsród pozycji od 1 do k znajduja sie wszystkie liczby od 1 do k. 
-- Napisac funkcje cp :: [Integer] -> [Integer], która dla permutacji podanej jako lista wyliczy wszystkie jej pozycje zamykajace. 
-- Przykładowo, cp [1,2,3] = [1,2,3], za´s cp [3,2,1] = [3].

-- cp :: [Integer] -> [Integer]
-- cp list = 

-- sprawdzian 2021/2022
-- Zadanie 1. Rozpatrzmy nastepujacy typ reprezentujacy graf skierowany:
-- type DirectedGraph = ([Int], Int -> Int -> Bool).
-- Pierwszy element pary to zbiór wierzchołków V, drugi to funkcja f taka,ze dla dowolnychx, y ∈ V mamy, ˙ze f x y = True ⇐⇒ 
-- x y jest (skierowana) krawedzia z x do y. Napisac funkcje
-- atDistance :: DirectedGraph -> Int -> Int -> [Int],
-- która dla wywołania atDistance g d v zwraca liste (byc moze z powtórzeniami) wszystkich wierzchołków, które w grafie g sa osiagalne z v droga 
-- długosci dokładnie d. Dla przypomnienia: w drodze wierzchołki moga sie powtarzac, zas długosc drogi to liczba jej krawedzi.

-- zad1 2021
type DirectedGraph = ([Int], Int -> Int -> Bool)

addNeighbours :: DirectedGraph -> [Int] -> Int -> [Int]
addNeighbours g list v = list ++ [u | u <- fst g, (snd g) v u]

atDistance :: DirectedGraph -> Int -> Int -> [Int]
atDistance g 0 v = [v]
atDistance g d v = foldl (addNeighbours g) [] (atDistance g (d-1) v)

-- for testing
adjFun ::  Int -> Int -> Bool
adjFun 1 2 = True
adjFun 1 3 = True
adjFun 3 1 = True
adjFun 2 4 = True
adjFun 5 2 = True
adjFun 3 5 = True
adjFun _ _ = False

graph :: DirectedGraph
graph = ([1..5], adjFun)

-- 2022 zad1
cztery :: [Int] -> Int
cztery = maximum . map length . filter (\list -> sum list `mod` 4 == 0) . spojnePodciagi
spojnePodciagi :: [Int] -> [[Int]]
spojnePodciagi [] = []
spojnePodciagi (x:xs) = foldl (\acc el -> (head acc ++ [el]) : acc) [[x]] xs ++ spojnePodciagi xs
-- 2022 zad2
zmapujWynik :: Double -> Double -> String
zmapujWynik maks x | x < 0 || x > maks = "Nieprawidlowe dane"
 | 0.0 <= (x / maks) * 100 && (x / maks) * 100 <= 50.0 = "2.0"
 | 50.0 < (x / maks) * 100 && (x / maks) * 100 <= 60.0 = "3.0"
 | 60.0 < (x / maks) * 100 && (x / maks) * 100 <= 70.0 = "3.5"
 | 70.0 < (x / maks) * 100 && (x / maks) * 100 <= 80.0 = "4.0"
 | 80.0 < (x / maks) * 100 && (x / maks) * 100 <= 90.0 = "4.5"
 | 90.0 < (x / maks) * 100 && (x / maks) * 100 <= 100.0 = "5.0"

usunSpacjeZPoczatku :: [Char] -> String
usunSpacjeZPoczatku x | head x == ' ' = usunSpacjeZPoczatku (tail x)
 | otherwise = x

usunSpacjeZKonca :: [Char] -> String
usunSpacjeZKonca x | last x == ' ' = usunSpacjeZKonca (init x)
 | otherwise = x

usunSpacje :: [Char] -> String
usunSpacje x = usunSpacjeZKonca (usunSpacjeZPoczatku x)

wynikiPara :: Double -> (String,Double) -> (String, String)
wynikiPara maks pair = (usunSpacje (fst pair), zmapujWynik maks (snd pair))

wyniki :: Double -> [(String, Double)] -> [(String, String)]
wyniki maks = map (wynikiPara maks)
--2022 zad3

-- Zadanie 3. Bluszcz skierowany to struktura danych, która pozwala na: dołączenie elementu (de), 
-- odczytanie elementu ostatnio dołączonego (oe), usunięcie elementu ostatnio dołączonego (ue),
-- podanie liczby elementów równych elementowi dołączonemu jako pierwszy (le)
-- oraz zamianę całej struktury na listę (bsk2l), przy czym dowolne dwa elementy dołączone
-- w następujących po sobie operacjach de muszą być sąsiadami na liście. Zamiana na listę powinna 
-- być wykonalna w czasie liniowym względem łącznej liczby elementów, natomiast pozostałe
-- operacje — w czasie stałym. Zdefiniować typ Bsk a, służący do przechowywania elementów
-- typu a w bluszczu skierowanym, oraz następujące funkcje, realizujące opisane wyżej operacje
-- z odpowiednią złożonością:
-- Funkcje oe, ue i le nie są zdefiniowane dla bluszczu pustego.
-- data Bsk a = Empty | Bsk [a] Integer a deriving (Show)
 
-- checker :: (Eq a, Num a) => a -> a -> Bool
-- checker first second = first == second
 
-- de :: (Eq a, Num a) => Bsk a -> a -> Bsk a
-- de Empty elem = Bsk [elem] 1 elem
-- de (Bsk elemList counter firstElem) elem 
--     | checker elem firstElem = Bsk (elem:elemList) (counter + 1) firstElem
--     | otherwise =  Bsk (elem:elemList) counter firstElem
 
-- oe :: Bsk a -> a
-- oe (Bsk (x:elemList) counter firstElem) = x
 
-- ue :: (Eq a, Num a) => Bsk a -> Bsk a
-- ue (Bsk (elem:elemList) counter firstElem) 
--     | checker elem firstElem = Bsk elemList (counter - 1) firstElem
--     | otherwise =  Bsk elemList counter firstElem
 
-- le :: Eq a => Bsk a -> Integer
-- le (Bsk elemList counter firstElement) = counter
 
-- bsk2l :: Bsk a -> [a]
-- bsk2l (Bsk elemList counter firstElem) = reverse elemList
