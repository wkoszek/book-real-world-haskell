{-- snippet myDrop.type --}
myDrop :: Int -> [a] -> [a]
{-- /snippet myDrop.type --}

{-- snippet myDrop --}
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)
{-- /snippet myDrop --}

{-
{-- snippet myDrop1 --}
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)
{-- /snippet myDrop1 --}
-}

{-- snippet myDrop2 --}
myDropX n xs = if n <= 0 || null xs then xs else myDropX (n - 1) (tail xs)
{-- /snippet myDrop2 --}

{-- snippet niceDrop --}
niceDrop n xs | n <= 0 = xs
niceDrop _ []          = []
niceDrop n (_:xs)      = niceDrop (n - 1) xs
{-- /snippet niceDrop --}
