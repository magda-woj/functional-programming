import Text.Read (readMaybe)

-- sumOfSqures :: [String] -> Int
-- sumOfSqures xs = (readMaybe::Int <$> xs)

sumList :: [Maybe Int] -> Int
sumList xs = foldl (maybePlus) 0 xs

maybePlus :: Int -> Maybe Int -> Int
maybePlus _ Nothing = 0
maybePlus x (Just y) = x + (Just y)