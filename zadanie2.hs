silnia :: Int -> Double
-- silnia n = foldl (*) 1 (take n [1..])
-- silnia n = (foldl (*) 1) (flip take [1..] n)
-- silnia n = (foldl (*) 1) ((flip take [1..]) n)
silnia = foldl (*) 1.flip take [1..]

jedenPrzezSilnia :: Int -> Double
-- jedenPrzezSilnia n = 1 / (silnia n)
-- jedenPrzezSilnia n = (/) 1 (silnia n)
jedenPrzezSilnia = (/) 1.silnia

tablicaEulera :: Int -> [Double]
-- tablicaEulera n = map jedenPrzezSilnia (flip take [1..] n)
tablicaEulera = (map jedenPrzezSilnia).(flip take [1..])

liczbaEulera :: Int -> Double
liczbaEulera = (foldl (+) 1).(tablicaEulera)
