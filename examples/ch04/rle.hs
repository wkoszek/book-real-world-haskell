runLength []     = []
runLength (x:xs) = helper (x,1) xs
    where helper (prev, count) [] = [(prev, count)]
          helper (prev, count) (x:xs)
              | x == prev = helper (prev, count + 1) xs
              | otherwise = (prev, count) : helper (x, 1) xs

runLength_foldr :: (Eq a, Integral b) => [a] -> [(a, b)]
runLength_foldr = foldr step []
    where step x [] = [(x,1)]
          step x ((y,n):ys) | x == y = (y,n+1):ys
                            | otherwise = (x,1):(y,n):ys
