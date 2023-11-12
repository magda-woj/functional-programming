import Data.Char (isLower)
import Data.Binary.Get (label)
-- zad1
maleLiteryString :: String -> String
maleLiteryString s = [x | x <- ['a'..'z'], elem x s]

maleLitery :: [String] -> [String]
maleLitery l = map maleLiteryString l

-- zad2
jestPalindromem :: String -> Bool
jestPalindromem s = reverse s == s

dlugoscPalindromow :: [String] -> Int
dlugoscPalindromow l = sum (map length (filter jestPalindromem l))

-- zad3
fiboPair :: (Integer, Integer) -> (Integer, Integer)
fiboPair (x, y) = (y, x+y)

fib :: (Integer,Integer) -> [(Integer,Integer)]
fib p = iterate fiboPair p

firsts :: [(a,b)] -> [a]
firsts xs = [x | (x,_) <- xs]

fibonacci :: [Integer]
fibonacci = firsts (fib (0, 1))

-- zad4
dlugosc :: [a] -> Int
dlugosc l = sum (map (\c -> 1) l)

-- -- zad5
addLetter :: Char -> String -> String
addLetter x s = [x] ++ s

slowaDlugosci :: Char -> Char -> Int -> [String]
slowaDlugosci x y 1 = [[x],[y]]
slowaDlugosci x y a = (map (x:) (slowaDlugosci x y (a-1))) ++ (map (y:) (slowaDlugosci x y (a-1)))

-- zad7
myZip :: [a] -> [b] -> [(a, b)]
myZip = zipWith (\x y -> (x,y))

myMap :: (a -> b) -> [a] -> [b]
myMap fun l = zipWith (\ x y -> fun(x)) l [0..] 

-- zad8
sieve :: Integer -> [Integer] -> [Integer]
sieve p l = if p*p > last l then l
else
    let k = filter (\x-> not (x `mod` p == 0)) l in
        p : sieve (head k) k


eratosthenes :: Integer -> [Integer]
eratosthenes n = sieve 2 [2..n]

