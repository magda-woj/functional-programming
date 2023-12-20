import System.Console.Haskeline (Interrupt)
import Distribution.Simple.Test (test)
import Distribution.SPDX (LicenseId(EPICS))
-- zad 1 Zdefiniować typ
-- data Student = Student {imie :: String,
-- nazwisko :: String,
-- nrAlbumu :: Int
-- }
-- jako instancję Show w taki sposób, aby kolejne wartości pól były wypisywane w kolejnych linijkach.

data Student = Student {imie :: String,
                        nazwisko :: String,
                        nrAlbumu :: Int
}
janKowalski = Student "Jan" "Kowalski" 1234567

instance Show Student
    where
        show(Student a b c) = a ++ "\n" ++ b ++ "\n" ++ show c

-- zad 2 Zdefiniować typ Calkowite przez Zero, następnik całkowitej oraz poprzednik całkowitej (podobnie, jak Naturalne na slajdach).
-- Napisać funkcję, która konwertuje Integer do typu Calkowite oraz drugą funkcję, które konwertuje Calkowite na Integer.

data Calkowite = Zero | Nastepnik Calkowite | Poprzednik Calkowite
    deriving (Show, Eq)

intDoCal :: Integer -> Calkowite
intDoCal 0 = Zero
intDoCal n = if n > 0 then Nastepnik (intDoCal (n-1)) else Poprzednik (intDoCal (n+1))

calDoInt :: Calkowite -> Integer
calDoInt Zero = 0
calDoInt (Poprzednik c) = calDoInt c -1
calDoInt (Nastepnik c) = calDoInt c +1

-- zad 3 Dla typu
-- data Tree a = Empty | Node a (Tree a) (Tree a)
-- Oraz drzewa z liczbami typu Integer zdefiniować funkcje, która sprawdza, czy wszystkie wierzchołki drzewa mają liczby parzyste.

data Tree a = Empty | Node a (Tree a) (Tree a)

drzewo :: Tree Integer
drzewo = Node 0 Empty Empty

isEven :: Tree Integer -> Bool
isEven Empty = True
isEven (Node n left right) = even n && isEven left && isEven right

-- zad 4 Dla typu
-- data Tree a = Empty | Node a (Tree a) (Tree a)
-- zdefiniować trzy funkcje, zwracające listę jego elementów w porządku odpowiednio preorder, inorder oraz postorder

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node n left right) = inorder left ++ [n] ++ inorder right

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node n left right) = [n] ++ preorder left ++ preorder right

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node n left right) = postorder left  ++ postorder right ++ [n]

testTree :: Tree Integer
testTree = Node 1 (Node 2 (Node 4 Empty Empty)(Node 5 Empty Empty)) (Node 3(Node 6 Empty Empty)(Node 7 Empty Empty))

-- zad 5 Zdefiniować typ odpowiadający liczbom całkowitym Gaussa (liczby zespolone, których część rzeczywista jest całkowita oraz część urojona jest całkowita).
-- Uczynić typ instancją Show oraz Num. Ponieważ w Num trzeba zdefiniować wartość bezwzględną oraz znak, zdefiniować pierwszą jako identyczność, 
-- a drugą jako stalerówną jeden. Funkcję show zdefiniować tak, aby liczba a + bi została wypisana jako a+bi

-- data Gauss = Gauss Integer Integer
 
-- instance Show Gauss where
--  show (Gauss a b) | b >= 0 = show(a) ++ "+" ++ show(b) ++ "i"
-- otherwise = show(a) ++ show(b) ++ "i"
 
-- instance Num Gauss where
--  fromInteger n = Gauss (fromInteger n) 0
--  negate (Gauss a b) = Gauss (-a) (-b)
--  abs = id
--  signum a = 1
--  (Gauss a b) + (Gauss c d) = Gauss (a + c) (b + d)
--  (Gauss a b) * (Gauss c d) = Gauss (a * c - b * d) (a * d + b * c)

 