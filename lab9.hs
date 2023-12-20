silnia :: Integer -> Integer
silnia 0 = 1
silnia n = n*silnia (n-1)

displayList :: [Integer] -> IO ()
displayList [] = return ()
displayList l = do
    print (head l)
    displayList (tail l)

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

main = do
    putStrLn "Podaj liczbe: "
    n <- getLine
    let m = read n :: Int
    displayList (take m fibonacci)
