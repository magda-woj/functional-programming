jestSamogloska :: Char -> Int
jestSamogloska 'a' = 1
jestSamogloska 'A' = 1
jestSamogloska 'e' = 1
jestSamogloska 'E' = 1
jestSamogloska 'i' = 1
jestSamogloska 'I' = 1
jestSamogloska 'o' = 1
jestSamogloska 'O' = 1
jestSamogloska 'u' = 1
jestSamogloska 'U' = 1
jestSamogloska 'y' = 1
jestSamogloska 'Y' = 1
jestSamogloska _ = 0

parzyscieSamoglosek :: String -> Bool
parzyscieSamoglosek s = even (sum (map jestSamogloska s))

odpowiadaj :: String -> IO()
odpowiadaj s = if parzyscieSamoglosek s
    then do
            putStrLn "tak!"
    else do

            putStrLn "nie."

main :: IO ()
main = do
            putStrLn "Zadaj pytanie:"
            s <- getLine
            odpowiadaj s
            -- odkomentowac ponizsze linie zeby moc odpalic w zwyklej konsoli i zeby program nie zamykal sie natychmiast po odpowiedzi na pytanie (bo czeka jeszcze na m)
            -- m <- getLine 
            -- return ()
