-- silnia :: Integer -> Integer
-- silnia 0 = 1
-- silnia n = n*silnia (n-1)

-- displayList :: [Integer] -> IO ()
-- displayList [] = return ()
-- displayList l = do
--     print (head l)
--     displayList (tail l)

-- fibonacci :: [Integer]
-- fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- main = do
--     putStrLn "Podaj liczbe: "
--     n <- getLine
--     let m = read n :: Int
--     displayList (take m fibonacci)

import System.IO
import System.Environment

countElem :: String -> Int
countElem = length

printCount :: Handle -> IO ()
printCount handle = do
  totalChars <- count handle 0
  putStrLn (show totalChars)

count :: Handle -> Int -> IO Int
count handle acc = do
  eof <- hIsEOF handle
  if eof
    then return acc
    else do
      line <- hGetLine handle
      let lineChars = countElem line
      count handle (acc + lineChars)

main :: IO ()
main = do
    fileHandle <- openFile "1.txt" ReadMode
    printCount fileHandle
    hClose fileHandle
