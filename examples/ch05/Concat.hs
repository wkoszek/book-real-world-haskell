import Prelude hiding (concat)

{-- snippet concat --}
concat :: [[a]] -> [a]
concat = foldr (++) []
{-- /snippet concat --}
