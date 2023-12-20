-- zad 1
atDistance :: ([a], a->a->Bool) -> Int -> a -> [a]
atDistance g 0 v = [v]
atDistance g d v = [y | x <- fst g, snd g v x, y <- atDistance g (d-1) x]

-- zad 2
fun6 :: Int -> [[Int]] -> Int
fun6 x lists = sum (map (fromEnum . elem x) lists)
fun7 :: Int -> [[Int]] -> [Int]
fun7 x lists = [fun6 y lists | y<-[1..x]]

-- zad 3
data Wr a = Wr Int Bool [[a]]

dg :: Wr a -> [a] -> Wr a
dg (Wr n _ gs) xs = Wr (n + 1) True (xs:gs) 

ug :: Wr a -> Wr a
ug (Wr n _ (g:gs)) = Wr (n - 1) False gs
ug (Wr n w []) = Wr n w []

de :: Wr a -> a -> Wr a
de (Wr n False (g:gs)) _ = Wr n False (g:gs)
de (Wr n True (g:gs)) x = Wr n True ((x:g):gs)

ue :: Wr a -> Wr a
ue (Wr n False gs) = Wr n False gs
ue (Wr n True ((x:g):gs)) = Wr n False (g:gs)

lg :: Wr a -> Integer
lg (Wr n _ _) = toInteger n

wr2l :: Wr a -> [a]
wr2l (Wr _ _ gs) = concat gs