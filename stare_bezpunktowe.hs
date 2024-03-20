dodajPunkty :: Integer -> [[Integer]] -> [Integer]
dodajPunkty = const ( foldl (zipWith (+)) [0,0..])

f :: [(Int, Int)] -> Int -> Int
-- f xs u = foldl (*) u (map (uncurry (-)) xs)
-- f xs = flip (foldl (*)) (map (uncurry (-)) xs) u
f = flip (foldl (*)).map (uncurry (-))

g :: [Int] -> [Int]
-- g list = map (\(a,b) -> 3 * (a+b)) (zip list [1..10])
-- g list = map (\(a,b) -> 3 * (a+b)) (flip zip [1..10] list)
g = map (\(a,b) -> 3 * (a+b)).flip zip [1..10]
