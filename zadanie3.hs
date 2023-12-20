data Wnr a = Wnr {
                segmenty :: [([a], (a,a))]
                } deriving Show

wp :: Wnr a
wp = Wnr {segmenty = []}

dl :: Num a => Wnr a -> a -> Wnr a
dl (Wnr ([])) x = Wnr ([([x], (x,x))])

-- dl (Wnr ((zaw, (min, maks)) : segmenty)) x = if (x < min) then (Wnr ((x:zaw, (x, maks)) : segmenty)) else
--                                                 if (x > maks) then (Wnr ((x:zaw, (min, x)) : segmenty)) else
--                                                     (Wnr ((x:zaw, (min, maks)) : segmenty))
    

ts :: Num a => Wnr a -> Wnr a
ts (Wnr (([], (x,y)):seg)) = (Wnr (([], (x, y)):seg))
ts (Wnr (seg)) = Wnr (([], (0,0)):seg)

maksimum :: ([a], (a,a)) -> a
maksimum (l, (x,y)) = y
wmax :: Ord a => Wnr a -> [a]
wmax (Wnr (seg)) = map maksimum seg

mini :: ([a], (a,a)) -> a
mini (l, (x,y)) = x
-- wmin :: Ord a => Wnr a -> [a]
-- wmin (Wnr (seg)) = map minimum seg