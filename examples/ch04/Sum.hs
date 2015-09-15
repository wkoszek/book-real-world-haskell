{-- snippet mySum --}
mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc _      = acc
{-- /snippet mySum --}

{-- snippet foldlSum --}
foldlSum xs = foldl step 0 xs
    where step acc x = acc + x
{-- /snippet foldlSum --}

{-- snippet niceSum --}
niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs
{-- /snippet niceSum --}

{-- snippet nicerSum --}
nicerSum :: [Integer] -> Integer
nicerSum = foldl (+) 0
{-- /snippet nicerSum --}
