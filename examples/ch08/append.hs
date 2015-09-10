{-# OPTIONS_GHC -fno-implicit-prelude #-}

{-- snippet append --}
(++) :: [a] -> [a] -> [a]

(x:xs) ++ ys = x : (xs ++ ys)
[]     ++ ys = ys
{-- /snippet append --}
