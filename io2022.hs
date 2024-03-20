--zad1 21/22
import System.IO
printSpaces :: Integer -> IO ()
printSpaces 0 = return ()
printSpaces x = do
    putStr " "
    printSpaces (x - 1)

printUp :: Integer -> Integer -> Integer -> IO ()
printUp 0 _ size = printMiddle size 
printUp left done size = do
    printSpaces (size - done - 1)
    putStr "/"
    printSpaces (done * 2)
    putStrLn "\\"
    printUp (left - 1) (done + 1) size

printTylda :: Integer -> IO ()
printTylda 0 = putStrLn ""
printTylda x = do
    putStr "~"
    printTylda (x - 1)
    
printMiddle :: Integer -> IO ()
printMiddle size = do
    printTylda (size * 2)
    printBottom size
printBottom :: Integer -> IO ()
printBottom size = do
    printSpaces (size - 1)
    putStrLn "||"

main :: IO ()
main = do
    n <- getLine
    let size = read n :: Integer
    printUp size 0 size