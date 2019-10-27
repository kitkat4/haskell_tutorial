minValueNotInList :: [Int] -> Int
minValueNotInList xs = minimum [x | x <- [minimum xs .. maximum xs], not (x `elem` xs)]

findRightTriangle :: Int -> [(Int, Int, Int)]
findRightTriangle l = [(a,b,c) | a <- [1..l], b <- [a..l], c <- [b..l], c^2 == a^2 + b^2]


