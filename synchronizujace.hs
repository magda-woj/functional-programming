import Prelude
import System.IO
import System.Environment

data DFA state = DFA {  states :: [state],
                        alphabet :: [Char],
                        transition:: state -> Char -> state
                    }

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (filter (x >) xs) ++ [x] ++ quickSort (filter (x <=) xs)

unique :: (Eq a) => [a] -> [a] -> [a]
unique [] _ = []
unique (x:xs) ls
        | x `elem` ls = unique xs ls
        | otherwise = x : unique xs (x:ls)

uniqueSorted :: (Eq a) => [a] -> [a]
uniqueSorted xs = foldr (\e xs -> if e /= head xs then e:xs else xs) [last xs] xs

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = let shorterSublists = sublists xs in
    [x:sublist | sublist <- shorterSublists] ++ shorterSublists

nonemptySublists :: [a] -> [[a]]
nonemptySublists xs = init (sublists xs)

powerAutomaton :: (Ord state) => DFA state -> DFA [state]
powerAutomaton (DFA sts al trans) = let powerStates = nonemptySublists sts in
    DFA powerStates al (\xs a -> uniqueSorted (quickSort (map (`trans` a) xs)))

neighbours :: Eq state => DFA state -> state -> [state]
neighbours (DFA sts al trans) st = unique (map (trans st) al) []

bfs :: Eq state => DFA state -> [state] -> [state] -> [state]
bfs _ _ [] = []
bfs auto visited (h:queue) = h : bfs auto (h : visited) (queue ++ filter (`notElem` visited) (neighbours auto h))

-- shortestWord :: DFA state -> state -> (State -> Bool) -> String
-- shortestWord (DFA automaton) source condition = 

trans :: Int -> Char -> Int
trans 0 _ = 1
trans 1 'a' = 1
trans 1 'b' = 2
trans 2 'a' = 0
trans 2 'b' = 1

someAutomaton :: DFA Int
someAutomaton = DFA [0,1,2] ['a','b'] trans

powSomeAutomaton :: DFA [Int]
powSomeAutomaton = powerAutomaton someAutomaton


-- constructDFAFromFile :: Handle -> DFA
-- constructDFAFromFile handle = do
--                                 n <- hGetLine handle
--                                 let numOfStates = read n :: Int


-- main = do
--         fileHandle <- openFi4le "automaton.txt" ReadMode
--         constructDFAFromFile fileHandle
--         hClose fileHandle
