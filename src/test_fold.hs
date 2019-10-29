
elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = foldr (\y acc -> acc || y == x) False xs

