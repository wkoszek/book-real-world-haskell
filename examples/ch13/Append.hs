import Prelude hiding ((++))

{-- snippet append --}
(++) :: [a] -> [a] -> [a]
(x:xs) ++ ys = x : xs ++ ys
_      ++ ys = ys
{-- /snippet append --}
