dodajCykl :: a -> [[a]] -> [[a]]
dodajCykl e cykle = (e : (init (head cykle))) : cykle

cykl :: [a] -> [[a]]
cykl xs = foldr dodajCykl [xs] (tail xs)

-- wydluz :: [String] -> String -> [String]
-- wysluz lista s = 

dobreCiagi :: Int -> [String]
dobreCiagi 0 = []
dobreCiagi 1 = ["abb", "abc", "acb", "acc", "bab", "bac", "cab", "cac", "bba", "bca", "cba", "cca"]
dobreCiagi n = []

