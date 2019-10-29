take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]


zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys


quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort [y | y <- xs, y < x]) ++ [x] ++ (quickSort [y | y <- xs, y >= x])

collatz :: Int -> [Int]
collatz 1 = [1]
collatz x
    | even x = x : collatz (x `div` 2)
    | otherwise = x : collatz (3 * x + 1)



numLongCollatz :: [Int] -> Int
numLongCollatz xs = length (filter isLong (map collatz xs))
                    where isLong ys = length ys > 15
                                      

                   
