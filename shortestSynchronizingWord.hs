-- README:
-- Poniższy program sprawdza czy dany deterministyczny automat skończony (DFA) posiada słowo synchronizujące, 
-- a jeśli tak, to wypisuje najkrótsze takie słowo.
-- Plik wejściowy to opis automatu. Automat musi być poprawnym DFA. W pierwszej linijce pliku wejściowego znajduje się liczba stanów n.
-- W drugiej linijce znajduje się alfabet, zawierający k symboli, dany jako linijka zawierająca wszystkie symbole alfabetu bez spacji pomiędzy symbolami
-- W trzeciej linijce znajduje się opis funkcji przejścia w następującym formacie:
-- [lista0, lista1, ... listan-1], gdzie
-- listai to lista k-elementowa, której elementy są postaci (c, m), gdzie c to Char - element alfabetu, a m to liczba od 0 do n-1
-- - numer takiego stanu, że funkcja przejścia posyła stan i do stanu m literą c, tzn: funkcjaPrzejscia i c = m. 
-- Musi być to poprawna funkcja przejścia dla DFA.
-- Przykładowe zawartości plików wejściowych znajdują się w komentarzu pod kodem źródłowym.
-- Program wypisuje na standardowe wyjście najkrótsze słowo synchronizujące, jeśli automat jest synchronizujący, a jeśli nie jest
-- - stringa "automaton isn't synchronizing".

-- uwaga: na potrzeby projektu w funkcji main i obsłudze pliku przyjmuje się, że stany numerowane są liczbami od 0 do n-1.
-- Jednak zaimplementowane funkcje znajdujące najkrótsze słowo synchronizujące wymagają jedynie, 
-- by typ reprezentujący stany był porównywalny.

import Prelude
import System.IO
import System.Environment

data DFA state = DFA {  states :: [state],
                        alphabet :: [Char],
                        transition:: state -> Char -> state
                    }
-- struktura symbolizująca skonczony automat detrministyczny: tablica stanów, alfabet, funkcja przejscia.
-- Zbiór stanów jest skończony i niepusty - niepusty, ponieważ DFA powinien mieć stan początkowy.
-- alfabet jest skończony
-- nie jest nam potrzebny stan początkowy ani zbiór stanów końcowych, więc ich nie uwzględniam, ale możnaby.

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (filter (x >) xs) ++ [x] ++ quickSort (filter (x <=) xs)
-- zwykły quicksort jak na zajęciach

uniqueSorted :: (Eq a) => [a] -> [a]
uniqueSorted xs = foldr (\e xs -> if e /= head xs then e:xs else xs) [last xs] xs
-- funkcja usuwająca duplikaty z posortowanej listy - jeżeli element jest różny od tego bezpośrednio za nim, to jest ostatnim wystąpieniem
-- i go "zostawiamy", jak jest równy to "usuwamy". foldr, i ostatnie wystąpienie zamiast foldl i pierwszego, bo head ma mniejszą złożoność
-- niż last

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = let shorterSublists = sublists xs in
    [x:sublist | sublist <- shorterSublists] ++ shorterSublists
-- podlisty jak na zajęciach żeby mieć podzbiory zbioru stanów do automatu potęgowego...

nonemptySublists :: [a] -> [[a]]
nonemptySublists xs = init (sublists xs)
-- ...tylko nie chcemy zbioru pustego

powerAutomaton :: (Ord state) => DFA state -> DFA [state]
powerAutomaton (DFA sts al trans) = let powerStates = nonemptySublists sts in
    DFA powerStates al (\xs a -> uniqueSorted (quickSort (map (`trans` a) xs)))
-- tworzymy automat potęgowy. Na zbiorach wystarczyłoby zmapować funkcję przejścia oryginalnego automatu, ale w liście musimy usunąc 
-- duplikaty i posortować bo stan [0,1] i stan [1,0] to powinno być to samo

-- zwykła lista sąsiadów do zwykłego bfs:
-- neighbours :: Eq state => DFA state -> state -> [state]
-- neighbours (DFA sts al trans) st = map (trans st) al

neighbours :: Eq state => DFA state -> (state, String) -> [(state, String)]
neighbours (DFA sts al trans) (st, word) =  map (\ c -> (trans st c, word ++ [c])) al
-- dla pary (stan, słowo) zwraca listę par (sąsiad stanu, słowo ++ [literka którą się przechodzi ze stanu do tego sąsiada])

-- zwykły bfs: lepiej na nim widać ideę, więc zostawiam jako komentarz do obrony. Potem go modyfikowałam żeby trackować słowo
-- bfs :: Eq state => DFA state -> [state] -> [state] -> [state]
-- bfs _ _ [] = []
-- -- jeśli kolejka jest pusta to kończymy
-- bfs automaton visited (h:queue)
-- -- "ściągamy" głowę kolejki
--     | h `notElem` visited = h : bfs automaton (h : visited) (queue ++ filter (`notElem` visited) (neighbours automaton h))
--     -- jeśli jest nieodwiedzona to dodajemy dodajemy do wyniku bfsa z nieodwiedzonymi sąsiadami dodanymi do kolejki
--     | otherwise = bfs automaton visited queue
--     -- jeśli jest odwidzona to po prostu ściągamy z kolejki i nas nie obchodzi

bfs :: Eq state => DFA state -> [state] -> [(state, String)] -> [(state, String)]
bfs _ _ [] = []
bfs automaton visited (h:queue)
    | fst h `notElem` visited = h : bfs automaton (fst h : visited) (queue ++ filter (\(st, word) -> st `notElem` visited) (neighbours automaton h))
    | otherwise = bfs automaton visited queue
-- działa tak jak bfs powyżej, tylko w kolejce i autpucie mamy pary (stan, słowo_którym_dochodzi_się_od_źródła_do_stanu)
-- wywoływany będzie na visited = [] i queue = [(źródło, "")]

pathWords :: Ord state => DFA state -> [([state], String)]
pathWords automaton = let powAuto = powerAutomaton automaton in
    bfs powAuto [] [(head (states powAuto), "")]
-- odpowiednie wywołanie napisanego wyżej bfsa żeby dostać ścieżki w automacie potęgowym automatu który jest argumentem.

synchronizingWords :: Ord state => DFA state -> [String]
synchronizingWords automaton = map snd (filter (\x -> length (fst x) == 1) (pathWords automaton))
-- zwraca tylko slowa synchronizujące, czyli te które są ścieżka w automacie potęgowym od zbioru wszystkich stanów do singletonu.

shorter :: String -> String -> String
shorter s1 s2 = if length s1 <= length s2 then s1 else s2
-- zwraca krótsze słowo a jak są równej długości to pierwsze.

shortestSynchronizingWord :: Ord state => DFA state -> String
shortestSynchronizingWord automaton = let synchro = synchronizingWords automaton in
    foldr shorter (head synchro) synchro
-- zwraca najkrótsze słowo synchronizujące - wywyływać tylko na automacie synchronizującym.
-- nie używam w projekcie, żeby nie wywoływać 2 razy synchronizing words, ale zostawiam kod bo czemu nie

shortestWord :: [String] -> String
shortestWord xs = foldr shorter (head xs) xs
-- zwraca najkrótsze słowo na niepustej liście słów

find :: (a -> Bool) -> [a] -> a
find condition xs = head (foldl (\list x -> if condition x then x:list else list) [] xs)
-- znajduje element na liście i go zwraca

constructTransition :: [[(Char, Int)]] ->  (Int -> Char -> Int)
constructTransition xs n c = snd (find (\pair -> fst pair == c) (xs !! n))
-- tworzy funkcje przejscia z listy podanej w pliku wejsciowym

rList :: String -> [[(Char, Int)]]
rList = read

main :: IO ()
main = do
        handle <- openFile "automaton.txt" ReadMode
        n <- hGetLine handle
        let numOfStates = read n :: Int
        alphabet <- hGetLine handle
        trans <- hGetLine handle
        hClose handle
        let synchro = synchronizingWords (DFA [0..numOfStates-1] alphabet (constructTransition (rList trans)))
        if null synchro
            then print "automaton isn't synchronizing"
            else print (shortestWord synchro)

-- zestawy testowe:
-- zestaw 1:
-- input:
-- 3
-- ab
-- [[('a', 1), ('b',1)], [('a', 1), ('b', 2)], [('a', 0), ('b', 1)]]
-- output: "aa"

-- zestaw 2:
-- input:
-- 4
-- ab
-- [[('a', 1), ('b', 1)], [('a', 1),('b', 2)], [('a', 2),('b', 3)],[('a', 3),('b', 0)]]
-- output:
-- "abbbabbba"

-- zestaw 3:
-- input:
-- 2
-- ab
-- [[('a', 0),('b', 0)],[('a', 1),('b', 1)]]
-- output:
-- "automaton isn't synchronizing"

-- zestaw 4:
-- input:
-- 1
-- a
-- [[('a',0)]]
-- output:
-- ""

-- zestaw 5:
-- input:
-- 1
-- a
-- [[('a',0),('b',0),('c',0)]]
-- output:
-- ""

-- zestaw 6 PUSTY ALFABET:
-- input:
-- 1

-- [[]]
-- output:
-- ""

-- zestaw 7 PUSTY ALFABET:
-- input:
-- 2

-- [[], []]
-- output: 
-- "automaton isn't synchronizing"

-- zestaw 8
-- input:
-- 8
-- rb
-- [[('r', 4),('b', 1)],[('r', 2),('b', 4)],[('r',3),('b', 6)],[('r', 7),('b', 0)],[('r', 7),('b', 5)],[('r',1), ('b', 2)],[('r', 3), ('b', 5)],[('r', 0), ('b', 6)]]
-- output:
-- "bbbbrrb"

-- zestaw 9
-- input:
-- 4
-- abcd
-- [[('a', 1), ('b', 0), ('c', 3), ('d', 2)], [('a', 1), ('c', 1), ('b', 0), ('d', 0)], [('a', 2), ('b', 3), ('c', 0), ('d', 1)], [('b', 3), ('d', 3), ('a', 2), ('c', 2)]]
-- output: "aca"

-- zestaw 10:
-- input:
-- 3
-- ab
-- [[('a', 1), ('b', 2)], [('a', 1), ('b', 1)], [('a', 2), ('b', 2)]]
-- output: "automaton isn't synchronizing"
-- 