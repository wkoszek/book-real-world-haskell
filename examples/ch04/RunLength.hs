runLength [] = []
runLength (x:xs) = runLength' 1 x xs
    where
      runLength' n a [] = [(n, a)]
      runLength' n a (x:xs)
          | a == x    = runLength' (n+1) a xs
          | otherwise = (n, a) : runLength' 1 x xs
